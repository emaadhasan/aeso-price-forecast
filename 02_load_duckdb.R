# 02_load_duckdb.R
#
# Reads every monthly CSV from data/raw/, combines them into one table, runs
# basic data quality checks, and loads the result into a DuckDB database at
# data/aeso.duckdb.
#
# Run 01_pull_data.R first. Run from the project root.

library(DBI) # standard database interface
library(duckdb) # DuckDB driver
library(dplyr) # data manipulation
library(readr) # CSV reading
library(lubridate) # dates and time zones

# Settings

RAW_DIR = "data/raw"
DB_PATH = "data/aeso.duckdb"
LOCAL_TZ = "America/Edmonton" # Alberta (Mountain) time

# Read and combine all monthly files

files = list.files(RAW_DIR, pattern = "^pool_price_.*\\.csv$", full.names = TRUE)
message("Found ", length(files), " monthly files")

# write_csv saves every timestamp in UTC, so the time_mt column in the files is
# really UTC. Local time (MST in winter, MDT in summer) is rebuilt from time_utc.
pool_price = files |>
  lapply(read_csv, show_col_types = FALSE) |>
  bind_rows() |>
  distinct(time_utc, .keep_all = TRUE) |> # drop any overlap between months
  arrange(time_utc) |>
  mutate(time_mt = with_tz(time_utc, LOCAL_TZ), hour_ending = hour(time_mt) + 1)

message("Combined rows: ", nrow(pool_price))

# Time zone check

# Compares the first row (January, MST) with the first July row (MDT).
# The offset from UTC should be 7 hours in winter and 6 in summer.
cat("\nUTC vs Mountain time check\n")
pool_price |>
  slice(c(1, which.max(month(time_mt) == 7))) |>
  mutate(offset_hours = as.numeric(difftime(time_utc, force_tz(time_mt, "UTC"), units = "hours"))) |>
  select(time_utc, time_mt, offset_hours) |>
  print()

# Data quality checks

cat("\nDate range\n")
print(range(pool_price$time_utc))

cat("\nRows per year\n")
pool_price |>
  count(year = year(time_mt)) |>
  print(n = Inf)

cat("\nMissing values per column\n")
print(colSums(is.na(pool_price)))

# Builds every hour that should exist between the first and last timestamp,
# then lists the ones that are not in the data.
cat("\nGaps in the hourly sequence\n")
expected = seq(min(pool_price$time_utc), max(pool_price$time_utc), by = "hour")
missing = setdiff(expected, pool_price$time_utc) # setdiff drops the date class
cat("Missing hours:", length(missing), "\n")
if (length(missing) > 0 && length(missing) <= 20) {
  print(as.POSIXct(missing, origin = "1970-01-01", tz = "UTC"))}

cat("\nPrice summary\n")
print(summary(pool_price$pool_price))

cat("\nHighest prices on record\n")
pool_price |>
  slice_max(pool_price, n = 5) |>
  select(time_mt, pool_price, forecast_pool_price) |>
  print()

# Write to DuckDB

con = dbConnect(duckdb(), dbdir = DB_PATH)

# DuckDB converts time-zone-aware columns to UTC on write, which would undo the
# local time rebuilt above. Relabelling time_mt as UTC keeps the Alberta clock
# reading as-is, so EXTRACT(hour FROM time_mt) returns the Alberta hour.
pool_price_db = pool_price |>
  mutate(time_mt = force_tz(time_mt, "UTC"))

dbWriteTable(con, "pool_price", pool_price_db, overwrite = TRUE)

# Read it back with SQL to confirm it loaded

cat("\nReading back from DuckDB with SQL\n")
dbGetQuery(con, "
  SELECT
    EXTRACT(year FROM time_mt) AS yr,
    COUNT(*) AS hours,
    ROUND(AVG(pool_price), 2) AS avg_price,
    ROUND(MAX(pool_price), 2) AS max_price,
    ROUND(100.0 * SUM(CASE WHEN pool_price > 100 THEN 1 ELSE 0 END) / COUNT(*), 2) AS pct_hours_over_100
  FROM pool_price
  GROUP BY yr
  ORDER BY yr") |>
  print()

dbDisconnect(con, shutdown = TRUE)
message("Saved to ", DB_PATH)
