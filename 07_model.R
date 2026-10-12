# 07_model.R
#
# Trains and evaluates 24-hour-ahead price models, issued at midnight for each
# hour of the coming day:
#   1. Naive baselines (yesterday, last week, yesterday's average)
#   2. Linear regression
#   3. Gradient boosting (xgboost) on 2021 onward
#   4. Gradient boosting on recent data only (June 2024 onward)
#   5. Gradient boosting on the ratio of price to last week's average
#   6. Spike classifier: the probability that price is over $100
# AESO's own forecast is reported alongside, but it is a shorter-horizon
# forecast, so it is context rather than a like-for-like comparison.
#
# Saves the test-set predictions to a "predictions" table for the dashboard.
#
# Run 06_features.R first. Run from the project root.

library(DBI) # standard database interface
library(duckdb) # DuckDB driver
library(dplyr) # data manipulation
library(xgboost) # gradient boosting

set.seed(42) # makes xgboost's random row and column sampling repeatable

# Settings

DB_PATH = "data/aeso.duckdb"

# Features that are known at midnight, before the day being forecast
FEATURES = c("hour_ending", "day_of_week", "month", "is_weekend",
  "forecast_ail",
  "wind_lag24", "solar_lag24", "wind_lag168", "wind_avg_30d",
  "price_lag24", "price_lag25", "price_lag48", "price_lag168",
  "price_avg_prev_day", "price_avg_prev_week", "price_sd_prev_week",
  "price_max_prev_day", "spike_rate_prev_week")

# Shared xgboost settings. reg:absoluteerror minimises MAE, so a handful of
# $999 hours can't dominate training the way they would with squared error.
PARAMS = list(objective = "reg:absoluteerror", eta = 0.05, max_depth = 6,
  subsample = 0.8, colsample_bytree = 0.8)

# Load data

con = dbConnect(duckdb(), dbdir = DB_PATH)
d = dbGetQuery(con, "SELECT * FROM model_data ORDER BY time_utc") |> as_tibble()
dbDisconnect(con, shutdown = TRUE)

train = filter(d, split == "train")
valid = filter(d, split == "valid")
test = filter(d, split == "test")

cat("train:", nrow(train), " valid:", nrow(valid), " test:", nrow(test), "\n")

# Evaluation

# Errors are reported separately for normal and spike hours, because a single
# average error hides everything that matters in this market. Rows where a
# model has no prediction are dropped, so check n when comparing models.
# bias is the average of prediction minus actual: positive means the model
# predicts too high on average.
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

# 1. Baselines

# Any model worth keeping has to beat these simple rules
results$lag24 = evaluate(test$pool_price, test$price_lag24, "Naive: same hour yesterday")
results$lag168 = evaluate(test$pool_price, test$price_lag168, "Naive: same hour last week")
results$prev = evaluate(test$pool_price, test$price_avg_prev_day, "Naive: yesterday's average")
results$aeso = evaluate(test$pool_price, test$forecast_pool_price, "AESO forecast (shorter horizon)")

# 2. Linear regression

# reformulate() builds the formula pool_price ~ hour_ending + day_of_week + ...
lin = lm(reformulate(FEATURES, "pool_price"), data = train)
results$lm = evaluate(test$pool_price, predict(lin, newdata = test), "Linear regression")

# 3. Gradient boosting

# xgboost needs a numeric matrix of features, wrapped in an xgb.DMatrix
to_matrix = function(df) {as.matrix(df[, FEATURES])}

dtrain = xgb.DMatrix(to_matrix(train), label = train$pool_price)
dvalid = xgb.DMatrix(to_matrix(valid), label = valid$pool_price)
dtest = xgb.DMatrix(to_matrix(test))

# Adds up to 2000 trees, but stops once the validation error hasn't improved
# for 50 rounds in a row and keeps the best round
fit = xgb.train(PARAMS, dtrain, nrounds = 2000,
  evals = list(valid = dvalid), 
  early_stopping_rounds = 50, print_every_n = 100)

pred_xgb = predict(fit, dtest)
results$xgb = evaluate(test$pool_price, pred_xgb, "XGBoost (2021 onward)")

# 4. Recent data only

# The grid and price levels have changed a lot, so older years may mislead.
# time_mt holds the Alberta clock reading labelled as UTC (see 02_load_duckdb.R),
# so it is compared with a UTC date here.
train_recent = filter(train, time_mt >= as.POSIXct("2024-06-01", tz = "UTC"))

fit_recent = xgb.train(PARAMS,
  xgb.DMatrix(to_matrix(train_recent), label = train_recent$pool_price),
  nrounds = 2000, evals = list(valid = dvalid),
  early_stopping_rounds = 50, print_every_n = 100)

pred_recent = predict(fit_recent, dtest)
results$xgb_recent = evaluate(test$pool_price, pred_recent, "XGBoost (2024-06 onward)")

# 5. Predict the ratio to last week's average

# Modelling price / last week's average removes most of the shift in price
# level between years. The prediction is then multiplied back up. Weeks with
# an average of $1 or less are dropped to avoid dividing by almost zero.
train_r = filter(train, price_avg_prev_week > 1)
valid_r = filter(valid, price_avg_prev_week > 1)

fit_ratio = xgb.train(PARAMS,
  xgb.DMatrix(to_matrix(train_r), label = train_r$pool_price / train_r$price_avg_prev_week),
  nrounds = 2000,
  evals = list(valid = xgb.DMatrix(to_matrix(valid_r),
    label = valid_r$pool_price / valid_r$price_avg_prev_week)),
  early_stopping_rounds = 50, print_every_n = 100)

pred_ratio = predict(fit_ratio, dtest) * test$price_avg_prev_week
results$xgb_ratio = evaluate(test$pool_price, pred_ratio, "XGBoost (ratio to weekly avg)")

# Results

cat("\nTest set results (2026)\n")
bind_rows(results) |>
  arrange(mae) |>
  print(n = Inf)

cat("\nWhat the models are working with\n")
cat("Test mean price:", round(mean(test$pool_price, na.rm = TRUE), 2), "\n")
cat("Test spike rate:", round(100 * mean(test$spike, na.rm = TRUE), 1), "%\n")

# Gain = how much each feature improved the model's splits, as a share of the total
cat("\nFeature importance (main model)\n")
xgb.importance(model = fit) |>
  head(12) |>
  print()

# 6. Spike classifier

# Same features, but the target is the 1/0 spike flag. binary:logistic outputs
# a probability between 0 and 1, and AUC measures how well it ranks spike
# hours above normal ones.
cls_params = list(objective = "binary:logistic", eval_metric = "auc",
  eta = 0.05, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8)

fit_cls = xgb.train(cls_params,
  xgb.DMatrix(to_matrix(train), label = train$spike),
  nrounds = 2000,
  evals = list(valid = xgb.DMatrix(to_matrix(valid), label = valid$spike)),
  early_stopping_rounds = 50, print_every_n = 100)

test$spike_prob = predict(fit_cls, dtest)

# AUC is the chance that a random spike hour gets a higher probability than a
# random normal hour (0.5 = guessing, 1 = perfect). Computed from the ranks of
# the predictions, which is the Mann-Whitney formula.
auc = function(y, p) {
  r = rank(p)
  n1 = sum(y == 1)
  n0 = sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)}

cat("\nSpike classifier\n")
cat("Test AUC:", round(auc(test$spike, test$spike_prob), 3), "\n")

# Calibration: if the model says 10 to 25%, roughly that share of those hours
# should actually spike
cat("\nObserved spike rate by predicted probability\n")
test |>
  mutate(bucket = cut(spike_prob, c(0, 0.05, 0.1, 0.25, 0.5, 1), include.lowest = TRUE)) |>
  group_by(bucket) |>
  summarise(
    hours = n(),
    actual_spike_rate = round(100 * mean(spike), 1),
    avg_price = round(mean(pool_price), 2)) |>
  print()

# Save predictions for the dashboard

out = test |>
  mutate(
    pred_price = pred_xgb,
    pred_price_recent = pred_recent) |>
  select(time_utc, time_mt, pool_price, spike, forecast_pool_price,
    pred_price, pred_price_recent, spike_prob, forecast_ail, net_load_actual)

con = dbConnect(duckdb(), dbdir = DB_PATH)
dbWriteTable(con, "predictions", out, overwrite = TRUE)
dbDisconnect(con, shutdown = TRUE)
message("Saved predictions table to ", DB_PATH)
