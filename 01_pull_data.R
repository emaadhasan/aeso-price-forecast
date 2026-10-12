# 01_pull_data.R
#
# Pulls hourly Alberta pool price data from the AESO Pool Price API and saves
# one CSV per calendar month in data/raw/ (e.g. data/raw/pool_price_2026-08.csv).
#
# The script is safe to re-run: months that are already complete on disk are
# skipped, so a re-run only fetches the months that are missing or unfinished.
#
# Setup (one time):
#   1. Sign up at https://developer-apim.aeso.ca/, subscribe to the public
#      API product, and copy your primary key.
#   2. Run usethis::edit_r_environ() and add the line
#        AESO_API_KEY=your_key_here
#      then save the file and restart R. The key never appears in this code.
#
# Run from the project root so the data/raw/ path resolves correctly.

library(httr2)  # HTTP requests
library(dplyr) # data manipulation
library(lubridate) # dates and time zones
library(readr) # CSV writing

# Settings

API_KEY = Sys.getenv("AESO_API_KEY")
BASE_URL = "https://apimgw.aeso.ca/public/poolprice-api/v1.1/price/poolPrice"
RAW_DIR = "data/raw"
START_DATE = as.Date("2016-01-01") # first month of history to pull
LOCAL_TZ = "America/Edmonton" # Alberta (Mountain) time

if (API_KEY == "") {stop("AESO_API_KEY not found. Add it to 
                         .Renviron and restart R.")}
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

# Helper functions

# Converts the API's list of hourly records into a data frame.
# Some fields (e.g. forecast_pool_price) can come back as NULL; a NULL would
# drop the column entirely, so they are replaced with NA first.
parse_rows = function(rows) {bind_rows(lapply(rows, function(r) {
    r[vapply(r, is.null, logical(1))] = NA 
    as_tibble(r)}))}

# Requests pool prices for one date range. Dates are "YYYY-MM-DD" strings.
# Returns one row per hour.
get_pool_price = function(start_date, end_date) { resp = request(BASE_URL) |>
    req_headers(`API-KEY` = API_KEY) |>
    req_url_query(startDate = start_date, endDate = end_date) |>
    req_retry(max_tries = 3) |> # retries on temporary failures (429, 503)
    req_perform()

  rows = resp_body_json(resp)$return$`Pool Price Report`

  if (length(rows) == 0) {stop("API returned no rows for ", start_date, " to ", end_date)}

  # UTC is kept as the main timestamp because it has no daylight-saving gaps
  # or repeats. Local time and hour ending are added for analysis.
  parse_rows(rows) |>
    transmute(
      time_utc = ymd_hm(begin_datetime_utc, tz = "UTC"),
      time_mt = with_tz(time_utc, LOCAL_TZ),
      hour_ending = hour(time_mt) + 1, # AESO convention: HE01 = 00:00-01:00
      pool_price = as.numeric(pool_price),
      forecast_pool_price = as.numeric(forecast_pool_price),
      rolling_30day_avg = as.numeric(rolling_30day_avg))}

# Returns TRUE if the saved file for a month already holds the full month.
# A file counts as complete if it was written after the month ended.
# This catches the case where a month was saved part-way through and would
# otherwise never be topped up.
month_is_complete = function(file, month_end) {file.exists(file) &&
    as.Date(file.mtime(file)) > month_end}

# Pulls one calendar month and saves it as a CSV, unless it is already complete.
pull_month = function(month_start) {
  month_end = ceiling_date(month_start, "month") - days(1)
  label = format(month_start, "%Y-%m")
  file = file.path(RAW_DIR, paste0("pool_price_", label, ".csv"))

  if (month_is_complete(file, month_end)) 
    {message("Skipping ", label, " (already saved)")
    return(invisible(NULL))}

  message("Pulling ", label, " ...")
  df = get_pool_price(format(month_start), format(min(month_end, today())))
  write_csv(df, file)
  Sys.sleep(1) # short pause between requests to go easy on the API
  invisible(df)}

# Main: Pull the full history

months = seq(START_DATE, floor_date(today(), "month"), by = "month")

# as.list() keeps each element as a Date. A plain for (m in months)` would
# strip the class and turn each month into a number.
for (m in as.list(months)) {
  tryCatch(pull_month(m),
    error = function(e) message("Failed ", format(m, "%Y-%m"), ": ", conditionMessage(e)))}

# Quick check

saved = list.files(RAW_DIR, pattern = "^pool_price_.*\\.csv$")
message("Done. ", length(saved), " of ", length(months), " monthly files saved in ", RAW_DIR, ".")
