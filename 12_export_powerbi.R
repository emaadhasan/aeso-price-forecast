# 12_export_powerbi.R
#
# Shapes the DuckDB tables into flat files Power BI can load directly, in
# data/powerbi/:
#   fact_hourly        one row per test-period hour: actual price, forecasts,
#                      the prediction band, errors and slicer columns
#   dim_date           one row per date, for Power BI's date features
#   history_daily      daily price, demand and generation for the full history
#   model_performance  the scorecard: MAE overall, normal hours and spike hours
#   calibration        predicted spike risk vs how often spikes happened
#
# Run 09_model2.R and 11_adaptive_intervals.R first. Run from the project root.

library(DBI) # standard database interface
library(duckdb) # DuckDB driver
library(dplyr) # data manipulation
library(readr) # CSV writing

# Settings

DB_PATH = "data/aeso.duckdb"
OUT_DIR = "data/powerbi"

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Queries

# Each export is one SQL query, so the same query feeds both the CSV and the
# Parquet file

# Fact table: one row per hour of the test period. The prediction band is the
# rolling-window conformal band from 11_adaptive_intervals.R, and the naive
# forecast comes from model_data2 so the dashboard has a baseline to beat.
SQL_FACT = "
  SELECT
    p.time_utc,
    p.time_mt,
    p.pool_price,
    p.spike,
    p.spike_prob,
    p.forecast_pool_price AS aeso_forecast,
    p.pred_point AS pred_price,
    p.pred_two_part,
    m.price_lag24 AS pred_naive,
    i.lo_roll AS pred_low,
    i.hi_roll AS pred_high,
    p.forecast_ail,
    p.cushion_lag24,
    p.temp_avg,

    -- Errors, pre-computed so the dashboard only has to average them
    ABS(p.pool_price - p.pred_point) AS abs_err_model,
    ABS(p.pool_price - p.forecast_pool_price) AS abs_err_aeso,
    ABS(p.pool_price - m.price_lag24) AS abs_err_naive,
    p.pred_point - p.pool_price AS err_signed,

    -- Columns the dashboard can slice on
    CASE WHEN p.pool_price > 100 THEN 'Spike (>$100)' ELSE 'Normal' END AS price_type,
    CASE
      WHEN p.spike_prob > 0.25 THEN 'High'
      WHEN p.spike_prob > 0.10 THEN 'Elevated'
      ELSE 'Low'
    END AS risk_band,
    CASE
      WHEN i.lo_roll IS NULL THEN NULL
      WHEN p.pool_price BETWEEN i.lo_roll AND i.hi_roll THEN 1
      ELSE 0
    END AS inside_interval
  FROM predictions2 p
  LEFT JOIN intervals_adaptive i ON p.time_utc = i.time_utc
  LEFT JOIN model_data2 m ON p.time_utc = m.time_utc
  ORDER BY p.time_utc"

# Date dimension: built from the full hourly history, so it covers every date
# in both the fact and history tables. Power BI's date features need a table
# with one row per date and no gaps.
SQL_DATES = "
  SELECT DISTINCT
    CAST(time_mt AS DATE) AS date,
    EXTRACT(year FROM time_mt) AS year,
    EXTRACT(quarter FROM time_mt) AS quarter,
    EXTRACT(month FROM time_mt) AS month_num,
    MONTHNAME(time_mt) AS month_name,
    EXTRACT(day FROM time_mt) AS day,
    DAYNAME(time_mt) AS day_name,
    EXTRACT(dow FROM time_mt) AS day_of_week,
    CASE WHEN EXTRACT(dow FROM time_mt) IN (0, 6) THEN 'Weekend' ELSE 'Weekday' END AS day_type
  FROM hourly
  ORDER BY date"

# History: daily grain keeps the file small while still showing the
# multi-year picture
SQL_HISTORY = "
  SELECT
    CAST(time_mt AS DATE) AS date,
    ROUND(AVG(pool_price), 2) AS avg_price,
    ROUND(MAX(pool_price), 2) AS max_price,
    ROUND(MEDIAN(pool_price), 2) AS median_price,
    ROUND(AVG(ail), 0) AS avg_demand,
    ROUND(AVG(wind), 0) AS avg_wind,
    ROUND(AVG(solar), 0) AS avg_solar,
    ROUND(AVG(net_load), 0) AS avg_net_load,
    SUM(CASE WHEN pool_price > 100 THEN 1 ELSE 0 END) AS spike_hours
  FROM hourly
  GROUP BY date
  ORDER BY date"

# Calibration: groups hours by predicted spike risk and shows how often they
# actually spiked. The number prefixes make the groups sort in order.
SQL_CALIBRATION = "
  SELECT
    CASE
      WHEN spike_prob > 0.50 THEN '5. >50%'
      WHEN spike_prob > 0.25 THEN '4. 25-50%'
      WHEN spike_prob > 0.10 THEN '3. 10-25%'
      WHEN spike_prob > 0.05 THEN '2. 5-10%'
      ELSE '1. <5%'
    END AS predicted_risk,
    COUNT(*) AS hours,
    ROUND(100.0 * AVG(spike), 1) AS actual_spike_pct,
    ROUND(AVG(pool_price), 2) AS avg_price
  FROM predictions2
  GROUP BY predicted_risk
  ORDER BY predicted_risk"

# Pull the tables

con = dbConnect(duckdb(), dbdir = DB_PATH)

fact = dbGetQuery(con, SQL_FACT)
dates = dbGetQuery(con, SQL_DATES)
history = dbGetQuery(con, SQL_HISTORY)
calibration = dbGetQuery(con, SQL_CALIBRATION)

cat("Fact rows:", nrow(fact), "\n")
cat("Date rows:", nrow(dates), "\n")
cat("History rows:", nrow(history), "\n")

# Model scorecard

# MAE overall, for normal hours and for spike hours, all on the same test
# hours. Hours where a forecast is missing are dropped for that row only, so
# check the hours column.
score = function(label, pred) {
  ok = !is.na(pred) & !is.na(fact$pool_price)
  a = fact$pool_price[ok]
  p = pred[ok]
  spike = a > 100
  tibble(
    model = label,
    hours = length(a),
    mae = round(mean(abs(a - p)), 2),
    mae_normal = round(mean(abs(a[!spike] - p[!spike])), 2),
    mae_spike = round(mean(abs(a[spike] - p[spike])), 2))}

model_perf = bind_rows(
  score("Naive: same hour yesterday", fact$pred_naive),
  score("XGBoost (24 hours ahead)", fact$pred_price),
  score("Two-part model", fact$pred_two_part),
  score("AESO forecast (shorter horizon)", fact$aeso_forecast))

cat("\nModel performance\n")
print(model_perf)

cat("\nCalibration\n")
print(calibration)

# Write CSV

# DuckDB timestamps come back into R labelled as UTC, and write_csv would add
# a "Z" to each one, which Power BI may convert to the computer's time zone.
# Writing them as plain text keeps the clock reading exactly as stored.
plain_time = function(df) {
  mutate(df, across(where(~ inherits(.x, "POSIXct")), ~ format(.x, "%Y-%m-%d %H:%M:%S", tz = "UTC")))}

write_csv(plain_time(fact), file.path(OUT_DIR, "fact_hourly.csv"))
write_csv(dates, file.path(OUT_DIR, "dim_date.csv"))
write_csv(history, file.path(OUT_DIR, "history_daily.csv"))
write_csv(model_perf, file.path(OUT_DIR, "model_performance.csv"))
write_csv(calibration, file.path(OUT_DIR, "calibration.csv"))

# Write Parquet

# DuckDB writes Parquet straight from the query, so no extra package is needed
# and timestamps keep DuckDB's own zone-free type
parquet = c(fact_hourly = SQL_FACT, dim_date = SQL_DATES, history_daily = SQL_HISTORY)

for (nm in names(parquet)) {
  dbExecute(con, sprintf("COPY (%s) TO '%s' (FORMAT PARQUET)",
    parquet[[nm]], file.path(OUT_DIR, paste0(nm, ".parquet"))))}

dbDisconnect(con, shutdown = TRUE)

cat("\nWritten to", OUT_DIR, "\n")
print(list.files(OUT_DIR))
