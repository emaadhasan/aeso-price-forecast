# 08_supply_features.R
#
# Part A: supply cushion from the capacity columns already in the CSD
#   generation files. Available capacity minus what demand needed is the
#   headroom the grid had, and it is the strongest supply-side driver of
#   Alberta price spikes.
# Part B: hourly temperature for Calgary and Edmonton from Environment and
#   Climate Change Canada (ECCC).
#
# Saves "capacity", "supply" and "weather" tables to the database.
#
# These tables hold actual values, not forecasts: the cushion uses actual
# wind, solar and demand, and the temperatures are observations.
#
# Run 05_generation.R first. Run from the project root.

library(DBI) # standard database interface
library(duckdb) # DuckDB driver
library(dplyr) # data manipulation
library(readr) # CSV reading
library(lubridate) # dates and time zones

# Settings

DB_PATH = "data/aeso.duckdb"
CSV_GLOB = "data/raw_gen/*.csv"
WEATHER_DIR = "data/raw_weather"
WEATHER_START_YEAR = 2021

# Same CSV options as 05_generation.R
READ_GEN = sprintf("read_csv('%s', union_by_name = true, quote = '\"', escape = '\"', sample_size = -1)", CSV_GLOB)

# ECCC station IDs for the two airports. IDs change when a station is replaced,
# so Part B prints the current candidates from ECCC's inventory as a check.
STATIONS = c(CALGARY = 50430, EDMONTON = 50149)

# Part A1: hourly capacity by fuel

con = dbConnect(duckdb(), dbdir = DB_PATH)

# "Maximum Capability" is the most each plant could produce that hour, so it
# drops when a plant is on outage. Same pivot as the generation table.
dbExecute(con, sprintf("
  CREATE OR REPLACE TABLE capacity AS
  SELECT
    \"Date (MST)\"::TIMESTAMP + INTERVAL 7 HOUR AS time_utc,
    SUM(\"Maximum Capability\") AS cap_total,
    SUM(CASE WHEN \"Fuel Type\" IN ('GAS', 'COAL', 'DUAL FUEL') THEN \"Maximum Capability\" ELSE 0 END) AS cap_thermal,
    SUM(CASE WHEN \"Fuel Type\" = 'GAS' THEN \"Maximum Capability\" ELSE 0 END) AS cap_gas,
    SUM(CASE WHEN \"Fuel Type\" = 'HYDRO' THEN \"Maximum Capability\" ELSE 0 END) AS cap_hydro,
    SUM(CASE WHEN \"Fuel Type\" = 'ENERGY STORAGE' THEN \"Maximum Capability\" ELSE 0 END) AS cap_storage
  FROM %s
  GROUP BY time_utc
  ORDER BY time_utc", READ_GEN))

cat("Capacity table\n")
dbGetQuery(con, "
  SELECT
    COUNT(*) AS hours,
    MIN(time_utc) AS first,
    MAX(time_utc) AS last,
    ROUND(AVG(cap_total), 0) AS avg_cap,
    ROUND(AVG(cap_thermal), 0) AS avg_thermal
  FROM capacity") |>
  print()

# Part A2: supply cushion

# Cushion = controllable capacity (thermal, hydro, storage) minus net load,
# i.e. how much spare capacity was left after wind and solar. A low cushion is
# when prices tend to run away. NULLIF avoids dividing by zero.
dbExecute(con, "
  CREATE OR REPLACE TABLE supply AS
  SELECT
    c.time_utc,
    c.cap_total,
    c.cap_thermal,
    c.cap_gas,
    c.cap_storage,
    h.net_load,
    c.cap_thermal + c.cap_hydro + c.cap_storage - h.net_load AS cushion_mw,
    100.0 * (c.cap_thermal + c.cap_hydro + c.cap_storage - h.net_load) / NULLIF(h.net_load, 0) AS cushion_pct
  FROM capacity c
  JOIN hourly h ON c.time_utc = h.time_utc")

# 10% bands, using FLOOR so each band starts at its label (e.g. 20 = 20 to 29.9%)
cat("\nDoes the cushion explain price?\n")
dbGetQuery(con, "
  SELECT
    FLOOR(s.cushion_pct / 10) * 10 AS cushion_band_pct,
    COUNT(*) AS hours,
    ROUND(AVG(h.pool_price), 2) AS avg_price,
    ROUND(MEDIAN(h.pool_price), 2) AS median_price,
    ROUND(100.0 * AVG(CASE WHEN h.pool_price > 100 THEN 1.0 ELSE 0.0 END), 1) AS pct_over_100
  FROM supply s
  JOIN hourly h ON s.time_utc = h.time_utc
  WHERE h.time_mt >= TIMESTAMP '2021-01-01'
    AND s.cushion_pct IS NOT NULL
  GROUP BY cushion_band_pct
  ORDER BY cushion_band_pct") |>
  as_tibble() |>
  print(n = Inf)

dbDisconnect(con, shutdown = TRUE)

# Part B: hourly temperature from Environment Canada

dir.create(WEATHER_DIR, recursive = TRUE, showWarnings = FALSE)

# Station check: lists the active hourly stations at the two airports, so you
# can confirm the IDs in STATIONS are still current
inv_file = file.path(WEATHER_DIR, "station_inventory.csv")
if (!file.exists(inv_file)) {
  download.file(
    "https://collaboration.cmc.ec.gc.ca/cmc/climate/Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv",
    inv_file, mode = "wb")}

inv = read_csv(inv_file, skip = 3, show_col_types = FALSE)

cat("\nCandidate weather stations\n")
inv |>
  filter(Province == "ALBERTA",
    grepl("CALGARY INT|EDMONTON INT", Name),
    `HLY Last Year` >= year(today())) |>
  select(Name, `Station ID`, `HLY First Year`, `HLY Last Year`) |>
  print()

# Downloads one CSV per station per month, which is how ECCC serves hourly
# data, then reads them all back. A month's file is only downloaded again if
# it was saved before that month ended (same idea as 01_pull_data.R).
get_weather = function(station_id, name) {
  out = list()
  months = seq(as.Date(paste0(WEATHER_START_YEAR, "-01-01")), floor_date(today(), "month"), by = "month")

  for (m in as.list(months)) {
    month_end = ceiling_date(m, "month") - days(1)
    f = file.path(WEATHER_DIR, sprintf("%s_%s.csv", name, format(m, "%Y_%m")))

    if (!(file.exists(f) && as.Date(file.mtime(f)) > month_end)) {
      url = sprintf(paste0("https://climate.weather.gc.ca/climate_data/bulk_data_e.html",
        "?format=csv&stationID=%s&Year=%d&Month=%d&Day=14",
        "&timeframe=1&submit=Download+Data"), station_id, year(m), month(m))
      try(download.file(url, f, quiet = TRUE, mode = "wb"), silent = TRUE)
      Sys.sleep(0.5)}

    # Everything is read as text: the flag columns are blank for most of a
    # month, so type guessing reads them as logical and then warns on the
    # first letter. Files under 500 bytes are failed downloads.
    if (file.exists(f) && file.size(f) > 500) {
      out[[f]] = read_csv(f, col_types = cols(.default = col_character()))}}

  bind_rows(out)}

# Keeps time, temperature and wind speed, with the station name added to the
# column names. The column names contain a degree symbol, which encodes
# differently across computers, so they are found by pattern instead.
# ECCC times are Local Standard Time, a fixed UTC-7 in Alberta all year.
tidy_weather = function(df, suffix) {
  nm = names(df)
  col_time = nm[grepl("^Date/Time", nm)][1]
  col_temp = nm[grepl("^Temp \\(", nm)][1]
  col_wind = nm[grepl("^Wind Spd", nm)][1]

  tibble(
    time_utc = ymd_hm(df[[col_time]], tz = "UTC") + hours(7),
    temp = suppressWarnings(as.numeric(df[[col_temp]])),
    wind_kmh = suppressWarnings(as.numeric(df[[col_wind]]))) |>
    filter(!is.na(time_utc)) |>
    distinct(time_utc, .keep_all = TRUE) |>
    rename_with(~ paste0(.x, "_", suffix), c(temp, wind_kmh))}

yyc = get_weather(STATIONS[["CALGARY"]], "CALGARY")
yeg = get_weather(STATIONS[["EDMONTON"]], "EDMONTON")

cat("\nRaw rows. Calgary:", nrow(yyc), " Edmonton:", nrow(yeg), "\n")

# full_join keeps every hour that either station has
weather = full_join(tidy_weather(yyc, "yyc"), tidy_weather(yeg, "yeg"), by = "time_utc") |>
  arrange(time_utc) |>
  mutate(
    temp_avg = rowMeans(cbind(temp_yyc, temp_yeg), na.rm = TRUE),
    # rowMeans returns NaN (not NA) when both stations are missing that hour,
    # and SQL's IS NULL does not catch NaN
    temp_avg = ifelse(is.nan(temp_avg), NA_real_, temp_avg),
    # Heating and cooling degrees: demand rises in both directions from about
    # 18 C, so raw temperature alone hides half the effect
    hdd = pmax(18 - temp_avg, 0),
    cdd = pmax(temp_avg - 18, 0))

cat("\nWeather table\n")
print(head(weather))
cat("Rows:", nrow(weather),
  " missing Calgary temp:", sum(is.na(weather$temp_yyc)),
  " missing Edmonton temp:", sum(is.na(weather$temp_yeg)), "\n")
print(summary(weather$temp_avg))

con = dbConnect(duckdb(), dbdir = DB_PATH)
dbWriteTable(con, "weather", as.data.frame(weather), overwrite = TRUE)

# 5 C bands. FLOOR also handles negatives correctly: -7 C goes in the -10 band.
cat("\nDoes temperature line up with demand?\n")
dbGetQuery(con, "
  SELECT
    FLOOR(w.temp_avg / 5) * 5 AS temp_band,
    COUNT(*) AS hours,
    ROUND(AVG(h.ail), 0) AS avg_demand,
    ROUND(AVG(h.pool_price), 2) AS avg_price,
    ROUND(100.0 * AVG(CASE WHEN h.pool_price > 100 THEN 1.0 ELSE 0.0 END), 1) AS pct_over_100
  FROM weather w
  JOIN hourly h ON w.time_utc = h.time_utc
  WHERE w.temp_avg IS NOT NULL
    AND isfinite(w.temp_avg)
    AND h.time_mt >= TIMESTAMP '2021-01-01'
  GROUP BY temp_band
  ORDER BY temp_band") |>
  as_tibble() |>
  print(n = Inf)

dbDisconnect(con, shutdown = TRUE)
message("Saved 'capacity', 'supply' and 'weather' tables to ", DB_PATH)
