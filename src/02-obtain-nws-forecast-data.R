# load packages and functions ---------------------------------------------
library(tidyverse)
source("./fun/nws_forecast_retrieve.R")
source("./fun/nws_forecast_parse.R")
source("./fun/station_pressure.R")
source("./fun/wetbulb_from_relhumid.R")


# load NDAWN stations table -----------------------------------------------
ndawn_stations <- read_csv("./data/tbl-ndawn-stations.csv")


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
    },
    .progress = "NWS data retrieval"
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
  # assign date and local time based on timezone
  nws_forecasts_parsed |>
  rowwise() |>
  mutate(
    start_time2 = with_tz(start_time, tzone = location_tz),
    start_time_local = format(
      start_time,
      format = "%I %p",
      tz = location_tz
    ) |>
      str_remove("^0+"),
    end_time_local = format(
      end_time,
      format = "%I %p",
      tz = location_tz
    ) |>
      str_remove("^0+"),
  ) |>
  ungroup() |>
  mutate(date = date(start_time2)) |>
  select(-start_time2) |>
  # select daytime hours only
  group_by(date, location_id) |>
  dplyr::filter(is_daytime == TRUE) |>
  mutate(n = n()) |>
  dplyr::filter(n == 12) |>
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
  ungroup() |>
  mutate(delta_t = round(delta_t, 1))

# calculate peak hourly delta T
daytime_peak_delta_t <- nws_forecast_data |>
  group_by(date, location_id) |>
  dplyr::filter(delta_t == max(delta_t, na.rm = TRUE)) |>
  dplyr::filter(wind_speed == max(wind_speed, na.rm = TRUE)) |>
  dplyr::filter(start_time == min(start_time, na.rm = TRUE)) |>
  select(
    forecast_type,
    forecast_effective,
    forecast_expires,
    location_id,
    location_tz,
    date,
    start_time,
    end_time,
    start_time_local,
    end_time_local,
    delta_t,
    wind_speed,
    wind_speed_unit,
    wind_direction
  )


# write output to csv -----------------------------------------------------
write_csv(
  nws_forecast_data |>
    select(
      forecast_type,
      forecast_effective,
      forecast_expires,
      location_id,
      location_tz,
      date,
      start_time,
      end_time,
      start_time_local,
      end_time_local,
      delta_t,
      wind_speed,
      wind_speed_unit,
      wind_direction
    ),
  "./results/tbl-forecast-delta-t-hourly.csv"
)
write_csv(daytime_peak_delta_t, "./results/tbl-forecast-delta-t.csv")
