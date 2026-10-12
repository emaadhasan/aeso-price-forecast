# 10_intervals.R
#
# Conformalized quantile regression (CQR): widens the p10 to p90 prediction
# band so it actually covers about 80% of hours.
#
# Raw quantile models like those in 09_model2.R learn the spread of prices in
# the training years. When the test period is more volatile, their band
# covers fewer than 80% of hours. CQR corrects the width using the data:
#   1. Train p10 and p90 quantile models on the training set
#   2. On the validation set, measure how far outside the band each actual
#      price fell (the "conformity scores")
#   3. Widen the band by the 80th percentile of those scores
#
# CQR's coverage guarantee assumes the validation and test hours behave alike.
# Prices drift over time and validation (Jul to Dec 2025) covers different
# seasons than test (2026), so treat the result as a well-calibrated band,
# not a guaranteed one. The coverage-by-month table shows where it holds.
#
# Saves the raw and calibrated bands to an "intervals" table.
#
# Run 09_model2.R first. Run from the project root.

library(DBI) # standard database interface
library(duckdb) # DuckDB driver
library(dplyr) # data manipulation
library(xgboost) # gradient boosting

set.seed(42) # makes xgboost's random row and column sampling repeatable

# Settings

DB_PATH = "data/aeso.duckdb"
ALPHA = 0.20   # 1 - target coverage, so 0.20 means an 80% band
NROUNDS = 150  # fixed number of trees (see "Train the quantile models")
PRICE_FLOOR = 0
PRICE_CAP = 999.99 # Alberta's pool price is limited to $0 to $999.99/MWh

# Same features as 09_model2.R
FEATURES = c("hour_ending", "day_of_week", "month", "is_weekend",
  "forecast_ail",
  "wind_lag24", "solar_lag24", "wind_lag168", "wind_avg_30d",
  "price_lag24", "price_lag25", "price_lag48", "price_lag168",
  "price_avg_prev_day", "price_avg_prev_week", "price_sd_prev_week",
  "price_max_prev_day", "spike_rate_prev_week",
  "cushion_lag24", "cushion_mw_lag24", "cushion_lag168",
  "cushion_avg_prev_week", "cap_thermal_lag24", "cap_total_lag24",
  "cap_thermal_vs_norm",
  "temp_avg", "hdd", "cdd", "wind_kmh_yyc")

BASE_PARAMS = list(eta = 0.05, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8)

# Load data

con = dbConnect(duckdb(), dbdir = DB_PATH)
d = dbGetQuery(con, "SELECT * FROM model_data2 ORDER BY time_utc") |> as_tibble()
dbDisconnect(con, shutdown = TRUE)

train = filter(d, split == "train")
valid = filter(d, split == "valid") # used only for calibration in this file
test = filter(d, split == "test")

to_matrix = function(df) {as.matrix(df[, FEATURES])}

dtrain = xgb.DMatrix(to_matrix(train), label = train$pool_price)
dvalid = xgb.DMatrix(to_matrix(valid), label = valid$pool_price)
dtest = xgb.DMatrix(to_matrix(test))

# Train the quantile models

# A fixed number of rounds instead of early stopping on the validation set.
# That set is about to be used for calibration, and CQR needs calibration data
# that played no part in fitting the models.
fit_q = function(q) {
  xgb.train(c(BASE_PARAMS, objective = "reg:quantileerror", quantile_alpha = q),
    dtrain, nrounds = NROUNDS, verbose = 0)}

m_lo = fit_q(ALPHA / 2)
m_hi = fit_q(1 - ALPHA / 2)

lo_valid = predict(m_lo, dvalid)
hi_valid = predict(m_hi, dvalid)
lo_test = predict(m_lo, dtest)
hi_test = predict(m_hi, dtest)

# Percentage of hours where the actual price landed inside the band
coverage = function(y, lo, hi) {round(100 * mean(y >= lo & y <= hi, na.rm = TRUE), 1)}

cat("Before calibration\n")
cat("Validation coverage:", coverage(valid$pool_price, lo_valid, hi_valid), "%\n")
cat("Test coverage:", coverage(test$pool_price, lo_test, hi_test), "%\n")
cat("Median test width: $", round(median(hi_test - lo_test), 2), "\n", sep = "")

# Symmetric CQR

# Score for each validation hour: how far outside the band the actual price
# fell, or a negative number if it was inside. Widening the band by the 80th
# percentile of these scores would have covered 80% of validation hours.
# (n + 1) instead of n is the small-sample correction that makes it a
# guarantee rather than an estimate.
scores = pmax(lo_valid - valid$pool_price, valid$pool_price - hi_valid)
scores = scores[!is.na(scores)]
n = length(scores)
k = ceiling((n + 1) * (1 - ALPHA))
qhat = sort(scores)[min(k, n)]

cat("\nSymmetric CQR\n")
cat("Widening each side by $", round(qhat, 2), "\n", sep = "")
cat("Test coverage:", coverage(test$pool_price, lo_test - qhat, hi_test + qhat), "%\n")
cat("Median width: $", round(median((hi_test + qhat) - (lo_test - qhat)), 2), "\n", sep = "")

# Asymmetric CQR

# Prices stop at $0 but run up to $999.99, so misses are much bigger on the
# high side. Each side is calibrated separately, allowing ALPHA / 2 misses on
# each, so the lower edge isn't pushed down just because spikes are large.
q_one_sided = function(s, a) {
  s = s[!is.na(s)]
  n = length(s)
  sort(s)[min(ceiling((n + 1) * (1 - a)), n)]}

qlo = q_one_sided(lo_valid - valid$pool_price, ALPHA / 2)
qhi = q_one_sided(valid$pool_price - hi_valid, ALPHA / 2)

# Clipped to the price limits, since a band beyond them is meaningless
lo_c = pmax(lo_test - qlo, PRICE_FLOOR)
hi_c = pmin(hi_test + qhi, PRICE_CAP)

cat("\nAsymmetric CQR\n")
cat("Lower side widened by $", round(qlo, 2), ", upper side by $", round(qhi, 2), "\n", sep = "")
cat("Test coverage:", coverage(test$pool_price, lo_c, hi_c), "%\n")
cat("Median width: $", round(median(hi_c - lo_c), 2), "\n", sep = "")

# Where does coverage hold, and where does it fail?

# Overall coverage can hide a band that works for normal hours but almost
# never contains a spike
cat("\nCoverage by hour type (asymmetric CQR)\n")
test |>
  mutate(
    lo = lo_c,
    hi = hi_c,
    inside = pool_price >= lo & pool_price <= hi,
    type = ifelse(spike == 1, "spike hours", "normal hours")) |>
  group_by(type) |>
  summarise(
    hours = n(),
    coverage_pct = round(100 * mean(inside), 1),
    median_width = round(median(hi - lo), 2)) |>
  print()

cat("\nCoverage by month\n")
test |>
  mutate(
    lo = lo_c,
    hi = hi_c,
    inside = pool_price >= lo & pool_price <= hi,
    mth = format(time_mt, "%Y-%m")) |>
  group_by(mth) |>
  summarise(
    hours = n(),
    coverage_pct = round(100 * mean(inside), 1),
    median_width = round(median(hi - lo), 2)) |>
  print(n = Inf)

# Save

out = test |>
  mutate(
    pred_lo_raw = lo_test,
    pred_hi_raw = hi_test,
    pred_lo = lo_c,
    pred_hi = hi_c) |>
  select(time_utc, time_mt, pool_price, spike,
    pred_lo_raw, pred_hi_raw, pred_lo, pred_hi)

con = dbConnect(duckdb(), dbdir = DB_PATH)
dbWriteTable(con, "intervals", as.data.frame(out), overwrite = TRUE)
dbDisconnect(con, shutdown = TRUE)
message("Saved intervals table to ", DB_PATH)
