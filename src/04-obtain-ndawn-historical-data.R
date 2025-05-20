# load packages and functions ---------------------------------------------
library(tidyverse)
source("./fun/ndawn_retrieve.R")
source("./fun/station_pressure.R")
source("./fun/wetbulb_from_relhumid.R")


# define start and end dates ----------------------------------------------
startdate <- Sys.Date() - days(1)
enddate <- Sys.Date() - days(1)


# load NDAWN stations table -----------------------------------------------
ndawn_stations <- read_csv("./data/tbl-NDAWN-stations.csv")


# retrieve NDAWN hourly data for each NDAWN station ----------------------
ndawn_data <-
  ndawn_stations |>
  pull(station_id) |>
  map(
    slowly(
      \(x)
        ndawn_retrieve(
          stn_id = x,
          ndawn_fields = c(
            "Avg Air Temp",
            "Avg Rel Hum",
            "Avg Wind Speed",
            "Max Wind Speed",
            "Avg Wind Dir"
          ),
          date_start = startdate,
          date_end = enddate
        ),
      rate = rate_delay(1)
    ),
    .progress = "NDAWN data retrieval"
  ) |>
  bind_rows()


# reconfigure NDAWN data -------------------------------------------------
ndawn_data <- ndawn_data |>
  # select daytime hours
  dplyr::filter(is_daytime == TRUE) |>
  # calculate hourly delta T
  rowwise() |>
  mutate(
    delta_t = temperature -
      wetbulb_from_relhumid(
        Ftemp = temperature,
        RHvalue = relative_humidity,
        MBpressure = station_pressure(
          elev_ft = elevation,
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
daytime_peak_delta_t <- ndawn_data |>
  mutate(
    date = as_date(starting_datetime),
    wind_speed_unit = "mph"
  ) |>
  group_by(date, station_name) |>
  dplyr::filter(delta_t == max(delta_t, na.rm = TRUE)) |>
  dplyr::filter(wind_speed == max(wind_speed, na.rm = TRUE)) |>
  dplyr::filter(starting_datetime == min(starting_datetime, na.rm = TRUE)) |>
  select(
    station_id,
    station_name,
    latitude,
    longitude,
    elevation,
    date,
    delta_t,
    wind_speed,
    wind_gust,
    wind_speed_unit,
    wind_direction
  )


# write output to csv -----------------------------------------------------
write_csv(
  daytime_peak_delta_t,
  "./results/tbl-historical-delta-t.csv",
  append = TRUE
)
