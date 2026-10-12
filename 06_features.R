# 06_features.R
#
# Builds the modelling table from "hourly": calendar features, lagged prices,
# rolling price statistics and lagged renewables, all computed in SQL with
# window functions. Then splits the data by time into train, valid and test.
#
# Forecast setting: standing at midnight, predict each of the next 24 hours.
# At that point every hour of the previous day is known, so any value from 24
# or more hours before the target hour is safe to use. A feature is only
# allowed if it would be known at midnight:
#   Allowed - AESO's published load forecast, the calendar, prices and
#             wind/solar from 24h+ ago, slow-moving capacity trends
#   Not allowed - the day's actual demand, wind, solar, net load or price
# Actual net load and demand are kept but labelled, for an upper-bound model.
#
# Run 05_generation.R first. Run from the project root.

library(DBI) # standard database interface
library(duckdb) # DuckDB driver
library(dplyr) # data manipulation

# Settings

DB_PATH = "data/aeso.duckdb"

con = dbConnect(duckdb(), dbdir = DB_PATH)

# Build the features table

# WITH base AS (...) is a temporary table that adds the calendar columns, so
# the main SELECT can use them. WINDOW w names the window (all rows in time
# order) once so every LAG can reuse it. The ROWS BETWEEN windows count rows,
# so they assume one row per hour with no gaps.
dbExecute(con, "
  CREATE OR REPLACE TABLE features AS
  WITH base AS (
    SELECT
      time_utc,
      time_mt,
      hour_ending,
      pool_price,
      forecast_pool_price,
      ail,
      forecast_ail,
      wind,
      solar,
      net_load,
      EXTRACT(dow FROM time_mt) AS day_of_week,
      EXTRACT(month FROM time_mt) AS month,
      EXTRACT(year FROM time_mt) AS year
    FROM hourly)
  SELECT
    time_utc,
    time_mt,
    year,

    -- Targets
    pool_price,
    CASE WHEN pool_price > 100 THEN 1 ELSE 0 END AS spike,

    -- Benchmark: AESO's own price forecast
    forecast_pool_price,

    -- Calendar (DuckDB numbers Sunday as 0 and Saturday as 6)
    hour_ending,
    day_of_week,
    month,
    CASE WHEN day_of_week IN (0, 6) THEN 1 ELSE 0 END AS is_weekend,

    -- Demand known in advance
    forecast_ail,

    -- Renewables, lagged a full day so nothing from the target day leaks in
    LAG(wind, 24) OVER w AS wind_lag24,
    LAG(solar, 24) OVER w AS solar_lag24,
    LAG(wind, 168) OVER w AS wind_lag168,

    -- 30-day average wind: a slow-moving stand-in for how much wind is installed
    AVG(wind) OVER (ORDER BY time_utc ROWS BETWEEN 720 PRECEDING AND 24 PRECEDING) AS wind_avg_30d,

    -- Price history
    LAG(pool_price, 24) OVER w AS price_lag24,
    LAG(pool_price, 25) OVER w AS price_lag25,
    LAG(pool_price, 48) OVER w AS price_lag48,
    LAG(pool_price, 168) OVER w AS price_lag168,

    -- Rolling price levels, all ending 24 hours before the target hour
    AVG(pool_price) OVER (ORDER BY time_utc ROWS BETWEEN 47 PRECEDING AND 24 PRECEDING) AS price_avg_prev_day,
    AVG(pool_price) OVER (ORDER BY time_utc ROWS BETWEEN 191 PRECEDING AND 24 PRECEDING) AS price_avg_prev_week,
    STDDEV(pool_price) OVER (ORDER BY time_utc ROWS BETWEEN 191 PRECEDING AND 24 PRECEDING) AS price_sd_prev_week,
    MAX(pool_price) OVER (ORDER BY time_utc ROWS BETWEEN 47 PRECEDING AND 24 PRECEDING) AS price_max_prev_day,

    -- Share of recent hours that were spikes: a simple regime indicator
    AVG(CASE WHEN pool_price > 100 THEN 1.0 ELSE 0.0 END)
      OVER (ORDER BY time_utc ROWS BETWEEN 191 PRECEDING AND 24 PRECEDING) AS spike_rate_prev_week,

    -- Not known at midnight: kept only for the upper-bound comparison
    net_load AS net_load_actual,
    ail AS ail_actual

  FROM base
  WINDOW w AS (ORDER BY time_utc)
  ORDER BY time_utc")

# Checks

cat("Rows and columns\n")
print(dbGetQuery(con, "SELECT COUNT(*) AS rows FROM features"))
print(dbListFields(con, "features"))

# The first week of data has no 168-hour lag, so those rows get dropped later
cat("\nRows lost to lags and missing data\n")
dbGetQuery(con, "
  SELECT
    COUNT(*) AS total,
    SUM(CASE WHEN price_lag168 IS NULL THEN 1 ELSE 0 END) AS no_price_lag168,
    SUM(CASE WHEN wind_lag24 IS NULL THEN 1 ELSE 0 END) AS no_wind_lag24,
    SUM(CASE WHEN pool_price IS NULL THEN 1 ELSE 0 END) AS no_target
  FROM features") |>
  print()

cat("\nSpike rate by year\n")
dbGetQuery(con, "
  SELECT year, COUNT(*) AS hours, ROUND(100.0 * AVG(spike), 1) AS pct_spike
  FROM features
  GROUP BY year
  ORDER BY year") |>
  as_tibble() |>
  print(n = Inf)

# t() flips the one-row result on its side so each feature gets its own line
cat("\nCorrelation of each feature with price (2023 onward)\n")
dbGetQuery(con, "
  SELECT
    ROUND(CORR(forecast_ail, pool_price), 3) AS forecast_ail,
    ROUND(CORR(net_load_actual, pool_price), 3) AS net_load_actual,
    ROUND(CORR(wind_lag24, pool_price), 3) AS wind_lag24,
    ROUND(CORR(price_lag24, pool_price), 3) AS price_lag24,
    ROUND(CORR(price_lag168, pool_price), 3) AS price_lag168,
    ROUND(CORR(price_avg_prev_day, pool_price), 3) AS price_avg_prev_day,
    ROUND(CORR(spike_rate_prev_week, pool_price), 3) AS spike_rate_prev_week,
    ROUND(CORR(forecast_pool_price, pool_price), 3) AS aeso_forecast
  FROM features
  WHERE year >= 2023") |>
  t() |>
  print()

# Train / valid / test split

# Split by time. The model must be tested on hours that come
# after everything it was trained on. Training starts in 2021 because the grid
# before then (coal, far less wind and solar) no longer looks like today's.
# The data ends at the last hour with generation data. The generation files
# are downloaded by hand and lag behind the API data, and hours without them
# would have no wind, solar or supply features.
dbExecute(con, "
  CREATE OR REPLACE TABLE model_data AS
  SELECT *,
    CASE
      WHEN time_mt < TIMESTAMP '2025-07-01' THEN 'train'
      WHEN time_mt < TIMESTAMP '2026-01-01' THEN 'valid'
      ELSE 'test'
    END AS split
  FROM features
  WHERE time_mt >= TIMESTAMP '2021-01-01'
    AND time_utc <= (SELECT MAX(time_utc) FROM generation)
    AND pool_price IS NOT NULL
    AND price_lag168 IS NOT NULL")

cat("\nSplit sizes\n")
dbGetQuery(con, "
  SELECT
    split,
    COUNT(*) AS hours,
    MIN(time_mt) AS starts,
    MAX(time_mt) AS ends,
    ROUND(AVG(pool_price), 2) AS avg_price,
    ROUND(100.0 * AVG(spike), 1) AS pct_spike
  FROM model_data
  GROUP BY split
  ORDER BY starts") |>
  as_tibble() |>
  print()

dbDisconnect(con, shutdown = TRUE)
message("Built tables 'features' and 'model_data'")
