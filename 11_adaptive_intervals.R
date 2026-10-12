# 11_adaptive_intervals.R
#
# Fixes the two problems found in 10_intervals.R:
#   1. Drift: one correction fitted on 2025 undershoots in 2026, because
#      conformal methods assume calibration and test hours behave alike.
#   2. Spike hours: the band covers most normal hours but few spike hours,
#      because an 80% overall target can be met almost entirely on the easy
#      hours.
#
# Three fixes, all of which a live system could actually run:
#   A. Rolling-window conformal: recalibrate every hour on the last 30 days
#   B. Adaptive conformal inference (ACI): also nudge the target miss rate up
#      or down depending on whether recent bands have been missing too often
#   C. Conditional conformal: calibrate low-risk and high-risk hours separately,
#      using the spike classifier's probability
#
# Every method only uses prices at least 24 hours old, so each band could have
# been issued at midnight like the rest of the project's forecasts.
#
# Saves all the bands to an "intervals_adaptive" table.
#
# Run 09_model2.R first. Run from the project root.

library(DBI)     # standard database interface
library(duckdb)  # DuckDB driver
library(dplyr)   # data manipulation
library(xgboost) # gradient boosting

set.seed(42) # makes xgboost's random row and column sampling repeatable

# Settings

DB_PATH = "data/aeso.duckdb"
ALPHA_TARGET = 0.20 # target miss rate, so 80% coverage
WINDOW = 720        # calibrate on the last 30 days of scores
LAG = 24            # only use scores at least a day old
GAMMA = 0.02        # how quickly ACI reacts to misses
RISK_CUT = 0.10     # spike probability that splits low-risk from high-risk hours
MIN_GROUP = 50      # fewest scores a risk group needs before it is used alone
NROUNDS = 150       # same fixed tree count as 10_intervals.R
PRICE_FLOOR = 0
PRICE_CAP = 999.99

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

# Load data and train the models

con = dbConnect(duckdb(), dbdir = DB_PATH)
d = dbGetQuery(con, "SELECT * FROM model_data2 ORDER BY time_utc") |> as_tibble()
dbDisconnect(con, shutdown = TRUE)

train = filter(d, split == "train")
valid = filter(d, split == "valid")
test = filter(d, split == "test")

to_matrix = function(df) {as.matrix(df[, FEATURES])}
dtrain = xgb.DMatrix(to_matrix(train), label = train$pool_price)

# Same p10 and p90 models as 10_intervals.R, plus a spike classifier for
# method C. All trained on the training set only.
fit_q = function(q) {
  xgb.train(c(BASE_PARAMS, objective = "reg:quantileerror", quantile_alpha = q),
    dtrain, nrounds = NROUNDS, verbose = 0)}

m_lo = fit_q(ALPHA_TARGET / 2)
m_hi = fit_q(1 - ALPHA_TARGET / 2)

fit_cls = xgb.train(c(BASE_PARAMS, objective = "binary:logistic", eval_metric = "auc"),
  xgb.DMatrix(to_matrix(train), label = train$spike),
  nrounds = NROUNDS, verbose = 0)

# Calibration stream

# Validation followed by test, in time order, so the first test hours can
# calibrate on the end of the validation period. Each hour gets the raw band,
# a spike probability, and a conformity score (how far outside the band the
# actual price fell, negative if inside).
stream = bind_rows(valid, test) |> arrange(time_utc)
dstream = xgb.DMatrix(to_matrix(stream))
stream$lo_raw = predict(m_lo, dstream)
stream$hi_raw = predict(m_hi, dstream)
stream$spike_prob = predict(fit_cls, dstream)
stream$score = pmax(stream$lo_raw - stream$pool_price, stream$pool_price - stream$hi_raw)

# Positions of the test hours within the stream
test_idx = (nrow(valid) + 1):nrow(stream)
n_test = length(test_idx)

# Rows of the stream to calibrate stream row i on: the WINDOW hours that end
# LAG hours before it
window_rows = function(i) {max(1, i - LAG - WINDOW + 1):(i - LAG)}

# Conformal widening: the (1 - a) quantile of the scores, with the same
# small-sample correction as 10_intervals.R
conf_q = function(s, a) {
  s = s[!is.na(s)]
  n = length(s)
  if (n == 0) {return(0)}
  sort(s)[min(ceiling((n + 1) * (1 - a)), n)]}

coverage = function(y, lo, hi) {round(100 * mean(y >= lo & y <= hi, na.rm = TRUE), 1)}

# A. Rolling-window conformal

# The same widening as symmetric CQR, but recomputed every hour from the last
# 30 days, so the band follows the market as it changes
lo_roll = numeric(n_test)
hi_roll = numeric(n_test)

for (j in seq_len(n_test)) {
  i = test_idx[j]
  q = conf_q(stream$score[window_rows(i)], ALPHA_TARGET)
  lo_roll[j] = max(stream$lo_raw[i] - q, PRICE_FLOOR)
  hi_roll[j] = min(stream$hi_raw[i] + q, PRICE_CAP)}

# B. Adaptive conformal inference

# The rolling window, plus a target miss rate (alpha) that moves with recent
# results: each miss lowers alpha, which widens the band, and each hit raises
# it slightly, which tightens it. On average alpha settles where 20% of bands
# miss. GAMMA sets how fast it reacts.
alpha = ALPHA_TARGET
lo_aci = numeric(n_test)
hi_aci = numeric(n_test)
alpha_path = numeric(n_test)

for (j in seq_len(n_test)) {
  i = test_idx[j]
  q = conf_q(stream$score[window_rows(i)], alpha)
  lo_aci[j] = max(stream$lo_raw[i] - q, PRICE_FLOOR)
  hi_aci[j] = min(stream$hi_raw[i] + q, PRICE_CAP)
  alpha_path[j] = alpha

  # Update using the band from 24 hours ago, whose outcome is now known.
  # alpha is kept between 0.02 and 0.50: without that, a run of misses drives
  # it below 0 and the band jumps to the widest score in the window. The
  # limits trade ACI's long-run guarantee for bands that stay usable.
  if (j > LAG) {
    y_known = stream$pool_price[test_idx[j - LAG]]
    missed = as.numeric(y_known < lo_aci[j - LAG] | y_known > hi_aci[j - LAG])
    alpha = min(max(alpha + GAMMA * (ALPHA_TARGET - missed), 0.02), 0.50)}}

# C. Conditional conformal by spike risk

# Each hour is calibrated only on past hours in the same risk group (spike
# probability above or below RISK_CUT), so high-risk hours get wide bands and
# low-risk hours stay narrow. If a group has too few past hours in the window,
# all hours are used instead.
lo_cond = numeric(n_test)
hi_cond = numeric(n_test)

for (j in seq_len(n_test)) {
  i = test_idx[j]
  rows = window_rows(i)
  same_group = (stream$spike_prob[rows] > RISK_CUT) == (stream$spike_prob[i] > RISK_CUT)
  hist = stream$score[rows][same_group]
  if (length(hist) < MIN_GROUP) {hist = stream$score[rows]}
  q = conf_q(hist, ALPHA_TARGET)
  lo_cond[j] = max(stream$lo_raw[i] - q, PRICE_FLOOR)
  hi_cond[j] = min(stream$hi_raw[i] + q, PRICE_CAP)}

# Results

y = stream$pool_price[test_idx]
sp = stream$spike[test_idx] == 1

# Coverage overall and for normal and spike hours separately, plus band width.
# A method only counts as better if it improves coverage without simply making
# every band enormous.
summarise_method = function(lo, hi, label) {
  tibble(
    method = label,
    coverage = coverage(y, lo, hi),
    coverage_normal = coverage(y[!sp], lo[!sp], hi[!sp]),
    coverage_spike = coverage(y[sp], lo[sp], hi[sp]),
    median_width = round(median(hi - lo), 2),
    mean_width = round(mean(hi - lo), 2))}

cat("Interval calibration on the 2026 test data (target 80%)\n")
bind_rows(
  summarise_method(stream$lo_raw[test_idx], stream$hi_raw[test_idx], "Raw quantile models"),
  summarise_method(lo_roll, hi_roll, "Rolling-window conformal"),
  summarise_method(lo_aci, hi_aci, "Adaptive conformal (ACI)"),
  summarise_method(lo_cond, hi_cond, "Conditional by spike risk")) |>
  print()

cat("\nHow far alpha drifted under ACI\n")
cat("start:", ALPHA_TARGET,
  " min:", round(min(alpha_path), 3),
  " max:", round(max(alpha_path), 3),
  " end:", round(tail(alpha_path, 1), 3), "\n")

cat("\nMonthly coverage, rolling-window method\n")
tibble(
  mth = format(stream$time_mt[test_idx], "%Y-%m"),
  inside = y >= lo_roll & y <= hi_roll,
  width = hi_roll - lo_roll) |>
  group_by(mth) |>
  summarise(
    hours = n(),
    coverage_pct = round(100 * mean(inside, na.rm = TRUE), 1),
    median_width = round(median(width), 2)) |>
  print(n = Inf)

# Save

out = tibble(
  time_utc = stream$time_utc[test_idx],
  time_mt = stream$time_mt[test_idx],
  pool_price = y,
  spike = stream$spike[test_idx],
  spike_prob = stream$spike_prob[test_idx],
  lo_raw = stream$lo_raw[test_idx],
  hi_raw = stream$hi_raw[test_idx],
  lo_roll, hi_roll, lo_aci, hi_aci, lo_cond, hi_cond)

con = dbConnect(duckdb(), dbdir = DB_PATH)
dbWriteTable(con, "intervals_adaptive", as.data.frame(out), overwrite = TRUE)
dbDisconnect(con, shutdown = TRUE)
message("Saved intervals_adaptive table to ", DB_PATH)
