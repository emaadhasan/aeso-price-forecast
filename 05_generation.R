# 05_generation.R
#
# Reads the per-asset CSD generation CSVs straight from disk with DuckDB,
# totals them by hour and fuel type, and adds wind, solar, gas, coal, hydro
# and net load to the "hourly" table.
#
# The generation files are downloaded manually from the AESO website as two
# zips ( since they aren't available through the API). You can set their location in
# OUTER_ZIPS below.
#
# The files have two timestamp columns:
#   Date (MST) - fixed UTC-7 all year, so no daylight saving ambiguity
#   Date (MPT) - local Alberta time, which shifts with daylight saving
# The MST column is used, and 7 hours are added to get UTC.
#
# Run 04_join_data.R first. Run from the project root.

library(DBI) # standard database interface
library(duckdb) # DuckDB driver
library(dplyr) # data manipulation

# Settings

GEN_DIR = "data/raw_gen"
CSV_GLOB = "data/raw_gen/*.csv"
DB_PATH = "data/aeso.duckdb"

# Where the two downloaded zips are. Edit these to match your computer.
OUTER_ZIPS = c("~/Downloads/Generation_Data-selected.zip",
  "~/Downloads/Hourly-selected.zip")

# DuckDB reads every CSV matching CSV_GLOB as one table. union_by_name lines
# up columns by name in case files differ, and sample_size = -1 makes DuckDB
# scan whole files to detect column types instead of guessing from the start.
READ_GEN = sprintf("read_csv('%s', union_by_name = true, quote = '\"', escape = '\"', sample_size = -1)", CSV_GLOB)

# Unzip the downloads

# Each downloaded zip holds about 25 more zips, each holding one CSV, so the
# files are unzipped twice. Skipped once the CSVs are already extracted.
dir.create(GEN_DIR, recursive = TRUE, showWarnings = FALSE)

if (length(list.files(GEN_DIR, pattern = "\\.csv$")) == 0) {
  tmp = file.path(tempdir(), "aeso_gen")
  dir.create(tmp, showWarnings = FALSE)

  for (z in path.expand(OUTER_ZIPS)) {
    if (!file.exists(z)) {stop("Zip not found: ", z, ". Fix the path in OUTER_ZIPS.")}
    message("Unzipping ", basename(z))
    unzip(z, exdir = tmp)}

  inner = list.files(tmp, pattern = "^CSD Generation .*\\.zip$", full.names = TRUE)
  message("Found ", length(inner), " inner zips")

  for (z in inner) {unzip(z, exdir = GEN_DIR)}
  unlink(tmp, recursive = TRUE)
} else {message("CSVs already extracted, skipping unzip")}

# Drop 2015. The price and load tables start in 2016, so those rows would only
# be thrown away by the join anyway.
invisible(file.remove(list.files(GEN_DIR, pattern = "2015", full.names = TRUE)))

csvs = list.files(GEN_DIR, pattern = "\\.csv$")
message(length(csvs), " CSV files ready")

con = dbConnect(duckdb(), dbdir = DB_PATH)

# Fuel type check

# The CASE statements below have to match these names exactly. If a new fuel
# type appears here, it still counts in total_gen but gets no column of its own.
cat("Fuel types\n")
dbGetQuery(con, sprintf("
  SELECT \"Fuel Type\" AS fuel, COUNT(*) AS rows, ROUND(AVG(\"Volume\"), 1) AS avg_mw
  FROM %s
  GROUP BY fuel
  ORDER BY rows DESC", READ_GEN)) |>
  as_tibble() |>
  print(n = Inf)

# Total generation by hour and fuel type

# Each CASE keeps a row's MW only if it matches that fuel, otherwise adds 0,
# which turns one long column of assets into one column per fuel.
dbExecute(con, sprintf("
  CREATE OR REPLACE TABLE generation AS
  SELECT
    \"Date (MST)\"::TIMESTAMP + INTERVAL 7 HOUR AS time_utc,
    SUM(CASE WHEN \"Fuel Type\" = 'WIND' THEN \"Volume\" ELSE 0 END) AS wind,
    SUM(CASE WHEN \"Fuel Type\" = 'SOLAR' THEN \"Volume\" ELSE 0 END) AS solar,
    SUM(CASE WHEN \"Fuel Type\" = 'GAS' THEN \"Volume\" ELSE 0 END) AS gas,
    SUM(CASE WHEN \"Fuel Type\" = 'COAL' THEN \"Volume\" ELSE 0 END) AS coal,
    SUM(CASE WHEN \"Fuel Type\" = 'HYDRO' THEN \"Volume\" ELSE 0 END) AS hydro,
    SUM(\"Volume\") AS total_gen
  FROM %s
  GROUP BY time_utc
  ORDER BY time_utc", READ_GEN))

cat("\nGeneration table\n")
dbGetQuery(con, "
  SELECT COUNT(*) AS hours, MIN(time_utc) AS first_hour, MAX(time_utc) AS last_hour
  FROM generation") |>
  print()

# Timestamp check

# Solar should be zero overnight and peak around midday. If the peak looks
# shifted by an hour, the files are hour-ending rather than hour-starting.
cat("\nAverage solar and wind by MST hour (2024 onward)\n")
dbGetQuery(con, "
  SELECT
    EXTRACT(hour FROM time_utc - INTERVAL 7 HOUR) AS mst_hour,
    ROUND(AVG(solar), 0) AS avg_solar_mw,
    ROUND(AVG(wind), 0) AS avg_wind_mw
  FROM generation
  WHERE time_utc >= '2024-01-01'
  GROUP BY mst_hour
  ORDER BY mst_hour") |>
  as_tibble() |>
  print(n = Inf)

# Join into the hourly table

# The columns from 04_join_data.R are listed by name so this
# script can be re-run.
# Net load = demand minus wind and solar, the part the controllable plants
# (gas, hydro, coal) have to supply. Hours with no generation data get NULL,
# rather than treating their wind and solar as zero.
dbExecute(con, "
  CREATE OR REPLACE TABLE hourly AS
  SELECT
    h.time_utc,
    h.time_mt,
    h.hour_ending,
    h.pool_price,
    h.forecast_pool_price,
    h.rolling_30day_avg,
    h.ail,
    h.forecast_ail,
    h.ail_forecast_error,
    g.wind,
    g.solar,
    g.gas,
    g.coal,
    g.hydro,
    g.total_gen,
    h.ail - g.wind - g.solar AS net_load
  FROM hourly h
  LEFT JOIN generation g ON h.time_utc = g.time_utc
  ORDER BY h.time_utc")

cat("\nCoverage after the join\n")
dbGetQuery(con, "
  SELECT
    EXTRACT(year FROM time_mt) AS yr,
    COUNT(*) AS hours,
    SUM(CASE WHEN wind IS NULL THEN 1 ELSE 0 END) AS missing_gen,
    ROUND(AVG(wind), 0) AS avg_wind,
    ROUND(AVG(solar), 0) AS avg_solar,
    ROUND(AVG(net_load), 0) AS avg_net_load
  FROM hourly
  GROUP BY yr
  ORDER BY yr") |>
  as_tibble() |>
  print(n = Inf)

# Does net load explain price better than demand?

# Same 1,000 MW bands as 04_join_data.R, using FLOOR so each band starts at
# its label (e.g. the 7000 band is 7,000 to 7,999 MW).
cat("\nAverage price by net load band\n")
dbGetQuery(con, "
  SELECT
    FLOOR(net_load / 1000) * 1000 AS net_load_band,
    COUNT(*) AS hours,
    ROUND(AVG(pool_price), 2) AS avg_price,
    ROUND(MEDIAN(pool_price), 2) AS median_price,
    ROUND(100.0 * SUM(CASE WHEN pool_price > 100 THEN 1 ELSE 0 END) / COUNT(*), 1) AS pct_over_100
  FROM hourly
  WHERE net_load IS NOT NULL
  GROUP BY net_load_band
  ORDER BY net_load_band") |>
  as_tibble() |>
  print(n = Inf)

dbDisconnect(con, shutdown = TRUE)
message("Added generation columns to 'hourly' in ", DB_PATH)
