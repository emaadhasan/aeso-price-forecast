# 09_model2.R
#
# Part A: rebuilds the feature table with the supply cushion, capacity and
#   weather from 08_supply_features.R, and makes the same train / valid / test
#   split as 06_features.R.
# Part B: better spike handling, all 24-hour-ahead forecasts issued at midnight:
#   1. Point model with the new features (plus the original features on the
#      same rows, to measure what the new features add)
#   2. Quantile models (p10 / p50 / p90) for a prediction interval
#   3. Two-part model: P(spike) x spike-hour price + P(no spike) x normal price
#
# Saves the test-set predictions to a "predictions2" table for the dashboard.
#
# What is known at midnight:
#   Capacity and cushion are lagged 24 hours, so they only use hours that have
#   already happened.
#   Temperature and Calgary wind speed are the actual observations for the
#   hour, standing in for a weather forecast. Day-ahead temperature forecasts
#   are accurate to a degree or two, so this is a "perfect forecast"
#   assumption, and results should be described that way.
#
# Run 07_model.R and 08_supply_features.R first. Run from the project root.

library(DBI) # standard database interface
library(duckdb) # DuckDB driver
library(dplyr) # data manipulation
library(xgboost) # gradient boosting

set.seed(42) # makes xgboost's random row and column sampling repeatable

# Settings

DB_PATH = "data/aeso.duckdb"

# The features used in 07_model.R
FEATURES_V1 = c("hour_ending", "day_of_week", "month", "is_weekend",
  "forecast_ail",
  "wind_lag24", "solar_lag24", "wind_lag168", "wind_avg_30d",
  "price_lag24", "price_lag25", "price_lag48", "price_lag168",
  "price_avg_prev_day", "price_avg_prev_week", "price_sd_prev_week",
  "price_max_prev_day", "spike_rate_prev_week")

# The original features plus supply and weather
FEATURES = c(FEATURES_V1,
  "cushion_lag24", "cushion_mw_lag24", "cushion_lag168",
  "cushion_avg_prev_week", "cap_thermal_lag24", "cap_total_lag24",
  "cap_thermal_vs_norm",
  "temp_avg", "hdd", "cdd", "wind_kmh_yyc")

# Shared xgboost settings. Each model adds its own objective.
BASE_PARAMS = list(eta = 0.05, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8)

# Part A: features v2

con = dbConnect(duckdb(), dbdir = DB_PATH)

# joined adds the supply and weather columns to each hour of the features
# table, then the main SELECT lags the supply columns the same way as
# 06_features.R. LEFT JOIN keeps every hour even where supply or weather is
# missing, and xgboost handles the resulting NAs on its own.
dbExecute(con, "
  CREATE OR REPLACE TABLE features2 AS
  WITH joined AS (
    SELECT
      f.*,
      s.cushion_pct,
      s.cushion_mw,
      s.cap_thermal,
      s.cap_total,
      w.temp_avg,
      w.hdd,
      w.cdd,
      w.wind_kmh_yyc
    FROM features f
    LEFT JOIN supply s ON f.time_utc = s.time_utc
    LEFT JOIN weather w ON f.time_utc = w.time_utc)
  SELECT
    *,

    -- Supply side, lagged a day so nothing from the target day leaks in
    LAG(cushion_pct, 24) OVER w AS cushion_lag24,
    LAG(cushion_mw, 24) OVER w AS cushion_mw_lag24,
    LAG(cap_thermal, 24) OVER w AS cap_thermal_lag24,
    LAG(cap_total, 24) OVER w AS cap_total_lag24,
    LAG(cushion_pct, 168) OVER w AS cushion_lag168,
    AVG(cushion_pct) OVER (ORDER BY time_utc ROWS BETWEEN 191 PRECEDING AND 24 PRECEDING) AS cushion_avg_prev_week,

    -- Thermal capacity a day ago relative to its 30-day norm: a stand-in for
    -- outages. Below 1 means more plants than usual were unavailable.
    LAG(cap_thermal, 24) OVER w
      / NULLIF(AVG(cap_thermal) OVER (ORDER BY time_utc ROWS BETWEEN 720 PRECEDING AND 24 PRECEDING), 0)
      AS cap_thermal_vs_norm

  FROM joined
  WINDOW w AS (ORDER BY time_utc)
  ORDER BY time_utc")

# Same rows and split dates as model_data in 06_features.R, so the results
# here are directly comparable with 07_model.R
dbExecute(con, "
  CREATE OR REPLACE TABLE model_data2 AS
  SELECT *,
    CASE
      WHEN time_mt < TIMESTAMP '2025-07-01' THEN 'train'
      WHEN time_mt < TIMESTAMP '2026-01-01' THEN 'valid'
      ELSE 'test'
    END AS split
  FROM features2
  WHERE time_mt >= TIMESTAMP '2021-01-01'
    AND time_utc <= (SELECT MAX(time_utc) FROM generation)
    AND pool_price IS NOT NULL
    AND price_lag168 IS NOT NULL")

cat("Coverage of the new features\n")
dbGetQuery(con, "
  SELECT
    split,
    COUNT(*) AS hours,
    SUM(CASE WHEN cushion_lag24 IS NULL THEN 1 ELSE 0 END) AS missing_cushion,
    SUM(CASE WHEN temp_avg IS NULL THEN 1 ELSE 0 END) AS missing_temp
  FROM model_data2
  GROUP BY split") |>
  as_tibble() |>
  print()

d = dbGetQuery(con, "SELECT * FROM model_data2 ORDER BY time_utc") |> as_tibble()
dbDisconnect(con, shutdown = TRUE)

# Part B: models

train = filter(d, split == "train")
valid = filter(d, split == "valid")
test = filter(d, split == "test")

# Builds the xgboost matrix from a chosen set of feature columns
to_matrix = function(df, features = FEATURES) {as.matrix(df[, features])}

dtrain = xgb.DMatrix(to_matrix(train), label = train$pool_price)
dvalid = xgb.DMatrix(to_matrix(valid), label = valid$pool_price)
dtest = xgb.DMatrix(to_matrix(test))

# Same scorecard as 07_model.R
evaluate = function(actual, pred, label) {
  ok = !is.na(pred) & !is.na(actual)
  a = actual[ok]
  p = pred[ok]
  spike = a > 100
  tibble(
    model = label,
    n = length(a),
    mae = round(mean(abs(a - p)), 2),
    rmse = round(sqrt(mean((a - p)^2)), 2),
    mae_normal = round(mean(abs(a[!spike] - p[!spike])), 2),
    mae_spike = round(mean(abs(a[spike] - p[spike])), 2),
    bias = round(mean(p - a), 2))}

results = list()
results$lag24 = evaluate(test$pool_price, test$price_lag24, "Naive: same hour yesterday")

# 1. Point models

# The original features on these same rows: the fair comparison for showing
# what the supply and weather features add
fit_v1 = xgb.train(c(BASE_PARAMS, objective = "reg:absoluteerror"),
  xgb.DMatrix(to_matrix(train, FEATURES_V1), label = train$pool_price),
  nrounds = 2000,
  evals = list(valid = xgb.DMatrix(to_matrix(valid, FEATURES_V1), label = valid$pool_price)),
  early_stopping_rounds = 50, print_every_n = 200)

results$v1 = evaluate(test$pool_price, predict(fit_v1, xgb.DMatrix(to_matrix(test, FEATURES_V1))),
  "XGBoost (original features)")

fit = xgb.train(c(BASE_PARAMS, objective = "reg:absoluteerror"), dtrain,
  nrounds = 2000, evals = list(valid = dvalid),
  early_stopping_rounds = 50, print_every_n = 200)

pred_point = predict(fit, dtest)
results$point = evaluate(test$pool_price, pred_point, "XGBoost + cushion/weather")

cat("\nFeature importance\n")
xgb.importance(model = fit) |>
  head(15) |>
  print()

# 2. Quantile models

# Each model predicts a different percentile of the price. p10 should sit
# below the actual price 10% of the time and p90 should sit above it 90% of
# the time, so together they give an 80% prediction interval.
quant = list()
for (q in c(0.1, 0.5, 0.9)) {
  quant[[as.character(q)]] = xgb.train(
    c(BASE_PARAMS, objective = "reg:quantileerror", quantile_alpha = q),
    dtrain, nrounds = 2000, evals = list(valid = dvalid),
    early_stopping_rounds = 50, print_every_n = 500)}

p10 = predict(quant[["0.1"]], dtest)
p50 = predict(quant[["0.5"]], dtest)
p90 = predict(quant[["0.9"]], dtest)

results$median = evaluate(test$pool_price, p50, "XGBoost median (p50)")

# Coverage should be close to 80%. Much lower means the band is too narrow
# and the model is overconfident.
cat("\nPrediction interval\n")
cat("Share of hours inside the p10 to p90 band:",
  round(100 * mean(test$pool_price >= p10 & test$pool_price <= p90, na.rm = TRUE), 1),
  "% (target 80%)\n")
cat("Median band width: $", round(median(p90 - p10, na.rm = TRUE), 2), "\n", sep = "")

# The three models are trained separately, so p10 can occasionally come out
# above p50 or p90. This counts how often.
cat("Hours where the quantiles cross:", sum(p10 > p50 | p50 > p90, na.rm = TRUE), "\n")

# 3. Two-part model

# A classifier gives the chance of a spike, and two separate models predict
# the price if there is a spike and if there isn't. Training a model on spike
# hours alone lets it learn how big spikes get, instead of being dragged down
# by the large majority of hours that are ordinary.
fit_cls = xgb.train(
  c(BASE_PARAMS, objective = "binary:logistic", eval_metric = "auc"),
  xgb.DMatrix(to_matrix(train), label = train$spike), nrounds = 2000,
  evals = list(valid = xgb.DMatrix(to_matrix(valid), label = valid$spike)),
  early_stopping_rounds = 50, print_every_n = 200)

train_hi = filter(train, spike == 1)
train_lo = filter(train, spike == 0)
valid_hi = filter(valid, spike == 1)
valid_lo = filter(valid, spike == 0)

# Fewer rounds for the spike model, since it has far less data to learn from
fit_hi = xgb.train(c(BASE_PARAMS, objective = "reg:absoluteerror"),
  xgb.DMatrix(to_matrix(train_hi), label = train_hi$pool_price),
  nrounds = 1000,
  evals = list(valid = xgb.DMatrix(to_matrix(valid_hi), label = valid_hi$pool_price)),
  early_stopping_rounds = 50, print_every_n = 500)

fit_lo = xgb.train(c(BASE_PARAMS, objective = "reg:absoluteerror"),
  xgb.DMatrix(to_matrix(train_lo), label = train_lo$pool_price),
  nrounds = 2000,
  evals = list(valid = xgb.DMatrix(to_matrix(valid_lo), label = valid_lo$pool_price)),
  early_stopping_rounds = 50, print_every_n = 500)

# Weighted blend: a 20% spike chance gives 0.2 x the spike price plus 0.8 x
# the normal price
prob = predict(fit_cls, dtest)
pred_two_part = prob * predict(fit_hi, dtest) + (1 - prob) * predict(fit_lo, dtest)
results$two_part = evaluate(test$pool_price, pred_two_part, "Two-part (classifier x magnitude)")

# Results

# AUC: the chance a random spike hour gets a higher probability than a random
# normal hour (see 07_model.R)
auc = function(y, p) {
  r = rank(p)
  n1 = sum(y == 1)
  n0 = sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)}

cat("\nTest results\n")
bind_rows(results) |>
  arrange(mae) |>
  print(n = Inf)

cat("\nSpike classifier AUC (with new features):", round(auc(test$spike, prob), 3), "\n")

cat("\nClassifier calibration\n")
test |>
  mutate(bucket = cut(prob, c(0, 0.05, 0.1, 0.25, 0.5, 1), include.lowest = TRUE)) |>
  group_by(bucket) |>
  summarise(
    hours = n(),
    actual_spike_rate = round(100 * mean(spike), 1),
    avg_price = round(mean(pool_price), 2)) |>
  print()

# Save predictions for the dashboard

out = test |>
  mutate(
    pred_point = pred_point,
    pred_p10 = p10,
    pred_p50 = p50,
    pred_p90 = p90,
    pred_two_part = pred_two_part,
    spike_prob = prob) |>
  select(time_utc, time_mt, pool_price, spike, forecast_pool_price,
    pred_point, pred_p10, pred_p50, pred_p90, pred_two_part, spike_prob,
    forecast_ail, cushion_lag24, temp_avg)

con = dbConnect(duckdb(), dbdir = DB_PATH)
dbWriteTable(con, "predictions2", as.data.frame(out), overwrite = TRUE)
dbDisconnect(con, shutdown = TRUE)
message("Saved predictions2 table to ", DB_PATH)
