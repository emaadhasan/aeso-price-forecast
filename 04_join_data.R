# 04_join_data.R
#
# Reads the monthly load (AIL) CSVs from data/raw_load/, loads them into
# DuckDB, joins them to the pool price table with SQL, and saves the combined
# hourly table back to the database as "hourly". Ends with a first look at how
# demand and time of day relate to price.
#
# Run 02_load_duckdb.R and 03_pull_load.R first. Run from the project root.

library(DBI) # standard database interface
library(duckdb) # DuckDB driver
library(dplyr) # data manipulation
library(readr) # CSV reading
library(lubridate) # dates and time zones

# Settings

RAW_LOAD_DIR = "data/raw_load"
DB_PATH = "data/aeso.duckdb"
RECENT_START = format(today() - years(3), "%Y-%m-%d") # start of the "last 3 years"

# Read and combine the load files

files = list.files(RAW_LOAD_DIR, pattern = "^load_.*\\.csv$", full.names = TRUE)
message("Found ", length(files), " load files")

load_data = files |>
  lapply(read_csv, show_col_types = FALSE) |>
  bind_rows() |>
  distinct(time_utc, .keep_all = TRUE) |> # drop any overlap between months
  arrange(time_utc)

message("Load rows: ", nrow(load_data))

# Data quality checks

cat("\nMissing values per column\n")
print(colSums(is.na(load_data)))

# AESO's load forecast may not go back as far as the actual load
cat("\nEarliest hour with a load forecast\n")
load_data |>
  filter(!is.na(forecast_ail)) |>
  slice(1) |>
  print()

cat("\nAIL summary (MW)\n")
print(summary(load_data$ail))

# Write to DuckDB and join

con = dbConnect(duckdb(), dbdir = DB_PATH)
dbWriteTable(con, "load", load_data, overwrite = TRUE)

# One row per hour with price, demand, and AESO's forecasts side by side.
# LEFT JOIN from pool_price so no price hours are dropped if load is missing.
dbExecute(con, "
  CREATE OR REPLACE TABLE hourly AS
  SELECT
    p.time_utc,
    p.time_mt,
    p.hour_ending,
    p.pool_price,
    p.forecast_pool_price,
    p.rolling_30day_avg,
    l.ail,
    l.forecast_ail,
    l.ail - l.forecast_ail AS ail_forecast_error
  FROM pool_price p
  LEFT JOIN load l ON p.time_utc = l.time_utc
  ORDER BY p.time_utc")

cat("\nJoined table: coverage by year\n")
dbGetQuery(con, "
  SELECT
    EXTRACT(year FROM time_mt) AS yr,
    COUNT(*) AS hours,
    SUM(CASE WHEN ail IS NULL THEN 1 ELSE 0 END) AS missing_ail,
    SUM(CASE WHEN forecast_ail IS NULL THEN 1 ELSE 0 END) AS missing_forecast_ail
  FROM hourly
  GROUP BY yr
  ORDER BY yr") |>
  print()

# A first look at demand and price

# Groups hours into 1,000 MW demand bands (e.g. 9,000 to 9,999 MW is the 9000
# band). FLOOR rounds down. For ex, a plain CAST to INTEGER would round to the nearest.
cat("\nAverage price by demand level\n")
dbGetQuery(con, "
  SELECT
    FLOOR(ail / 1000) * 1000 AS ail_band,
    COUNT(*) AS hours,
    ROUND(AVG(pool_price), 2) AS avg_price,
    ROUND(MEDIAN(pool_price), 2) AS median_price,
    ROUND(100.0 * SUM(CASE WHEN pool_price > 100 THEN 1 ELSE 0 END) / COUNT(*), 1) AS pct_over_100
  FROM hourly
  WHERE ail IS NOT NULL
  GROUP BY ail_band
  ORDER BY ail_band") |>
  as_tibble() |>
  print(n = Inf)

# The last 3 years only, since the market has changed a lot since 2016.
cat("\nAverage price by hour of day (last 3 years)\n")
dbGetQuery(con, sprintf("
  SELECT
    hour_ending,
    ROUND(AVG(pool_price), 2) AS avg_price,
    ROUND(MEDIAN(pool_price), 2) AS median_price,
    ROUND(AVG(ail), 0) AS avg_ail
  FROM hourly
  WHERE time_mt >= TIMESTAMP '%s'
  GROUP BY hour_ending
  ORDER BY hour_ending", RECENT_START)) |>
  as_tibble() |>
  print(n = Inf)

dbDisconnect(con, shutdown = TRUE)
message("Saved joined table 'hourly' to ", DB_PATH)
