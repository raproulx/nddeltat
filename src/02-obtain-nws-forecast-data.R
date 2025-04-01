# load packages and functions ---------------------------------------------
library(tidyverse)
source("./fun/nws_forecast_retrieve.R")
source("./fun/nws_forecast_parse.R")
source("./fun/station_pressure.R")
source("./fun/wetbulb_from_relhumid.R")


# load NDAWN stations table -----------------------------------------------
ndawn_stations <- read_csv("./data/01-NDAWN-stations-table.csv")


# retrieve NWS hourly forecast for each NDAWN station ---------------------
nws_forecasts <-
  ndawn_stations |>
  select(latitude, longitude, station_id) |>
  pmap(
    \(...) {
      x <- tibble(...)
      nws_forecast_retrieve(
        "hourly",
        latitude = x |> pull(latitude),
        longitude = x |> pull(longitude),
        output_id = x |> pull(station_id)
      )
    }
  )


# parse NWS forecast data -------------------------------------------------
# parse NWS JSON outputs
nws_forecasts_parsed <-
  nws_forecasts |>
  map(
    \(x) {
      nws_forecast_parse(x)
    }
  ) |>
  bind_rows() |>
  mutate(location_id = as.integer(location_id))

# reconfigure parsed data
nws_forecast_data <-
  # adjust times to local timezone
  nws_forecasts_parsed |>
  left_join(
    ndawn_stations |> select(station_id, timezone),
    by = join_by(location_id == station_id)
  ) |>
  rowwise() |>
  mutate(across(where(is.timepoint), ~ with_tz(.x, tzone = timezone))) |>
  ungroup() |>
  # select daytime hours only
  mutate(date = date(start_time)) |>
  group_by(date, location_id) |>
  mutate(n = n()) |>
  filter(n == 24 & is_daytime == TRUE) |>
  # calculate hourly delta T
  rowwise() |>
  mutate(
    delta_t = temperature -
      wetbulb_from_relhumid(
        Ftemp = temperature,
        RHvalue = relative_humidity_value,
        MBpressure = station_pressure(
          elev_m = location_elev_m,
          altimeter_setting = 1013.25,
          altimiter_setting_unit = "mb",
          output_unit = "mb"
        ),
        output_unit = "F"
      )
  ) |>
  group_by(date, location_id)

# calculate peak hourly delta T
daytime_peak_delta_t <- nws_forecast_data |>
  filter(delta_t == max(delta_t, na.rm = TRUE)) |>
  filter(wind_speed == max(wind_speed, na.rm = TRUE)) |>
  filter(start_time == min(start_time, na.rm = TRUE)) |>
  select(
    forecast_type,
    forecast_effective,
    forecast_expires,
    location_id,
    date,
    start_time,
    end_time,
    delta_t,
    wind_speed,
    wind_speed_unit,
    wind_direction
  )


# write output to csv -----------------------------------------------------
write_csv(daytime_peak_delta_t, "./results/tbl-forecast-delta-t.csv")
