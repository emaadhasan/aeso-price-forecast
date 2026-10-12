# 03_pull_load.R
#
# Pulls hourly Alberta Internal Load (AIL) from the AESO Actual Forecast API,
# both the actual load and AESO's forecast of it, and saves one CSV per
# calendar month in data/raw_load/ (e.g. data/raw_load/load_2026-08.csv).
#
# The script is safe to re-run: months that are already complete on disk are
# skipped, so a re-run only fetches the months that are missing or unfinished.
#
# Setup (one time):
#   Subscribe to "Actual Forecast Report - v1" on the AESO API portal. It uses
#   the same AESO_API_KEY in .Renviron as 01_pull_data.R.
#
# Run from the project root so the data/raw_load/ path resolves correctly.

library(httr2) # HTTP requests
library(dplyr) # data manipulation
library(lubridate) # dates and time zones
library(readr) # CSV writing

# Settings

API_KEY = Sys.getenv("AESO_API_KEY")
BASE_URL = "https://apimgw.aeso.ca/public/actualforecast-api/v1/load/albertaInternalLoad"
RAW_DIR = "data/raw_load"
START_DATE = as.Date("2016-01-01") # first month of history to pull

if (API_KEY == "") {stop("AESO_API_KEY not found. Add it to .Renviron and restart R.")}
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

# Helper functions

# Converts the API's list of hourly records into a data frame.
# NULL fields are replaced with NA so every row keeps the same columns.
parse_rows = function(rows) {bind_rows(lapply(rows, function(r) {
    r[vapply(r, is.null, logical(1))] = NA
    as_tibble(r)}))}

# Requests actual and forecast load for one date range. Dates are "YYYY-MM-DD"
# strings. Returns one row per hour.
get_load = function(start_date, end_date) {resp = request(BASE_URL) |>
    req_headers(`API-KEY` = API_KEY) |>
    req_url_query(startDate = start_date, endDate = end_date) |>
    req_retry(max_tries = 3) |> # retries on temporary failures (429, 503)
    req_perform()

  rows = resp_body_json(resp)$return$`Actual Forecast Report`

  if (length(rows) == 0) {stop("API returned no rows for ", start_date, " to ", end_date)}

  # Only UTC time is kept here. This table is joined to pool prices on
  # time_utc, which already has the local time columns.
  parse_rows(rows) |>
    transmute(
      time_utc = ymd_hm(begin_datetime_utc, tz = "UTC"),
      ail = as.numeric(alberta_internal_load),
      forecast_ail = as.numeric(forecast_alberta_internal_load))}

# Returns TRUE if the saved file for a month already holds the full month.
# A file counts as complete if it was written after the month ended.
month_is_complete = function(file, month_end) {file.exists(file) &&
  as.Date(file.mtime(file)) > month_end}

# Pulls one calendar month and saves it as a CSV, unless it is already complete.
pull_month = function(month_start) {
  month_end = ceiling_date(month_start, "month") - days(1)
  label = format(month_start, "%Y-%m")
  file = file.path(RAW_DIR, paste0("load_", label, ".csv"))

  if (month_is_complete(file, month_end)) {
    message("Skipping ", label, " (already saved)")
    return(invisible(NULL))}

  message("Pulling ", label, " ...")
  df = get_load(format(month_start), format(min(month_end, today())))
  write_csv(df, file)
  Sys.sleep(1) # short pause between requests to go easy on the API
  invisible(df)}

# Main: Pull the full history

months = seq(START_DATE, floor_date(today(), "month"), by = "month")

# as.list() keeps each element as a Date. A plain `for (m in months)` would
# strip the class and turn each month into a number.
for (m in as.list(months)) {
  tryCatch(pull_month(m),
    error = function(e) message("Failed ", format(m, "%Y-%m"), ": ", conditionMessage(e)))}

# Quick check

saved = list.files(RAW_DIR, pattern = "^load_.*\\.csv$")
message("Done. ", length(saved), " of ", length(months), " monthly files saved in ", RAW_DIR, ".")
