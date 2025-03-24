source("fun/wetbulb_from_relhumid.R")
source("fun/station_pressure.R")
library(tidyverse)
library(ggplot2)

dat <- read_csv("data/00-carrington-test-data.csv") |>
  rowwise() |>
  mutate(
    deltat_f = temp_avg_f -
      wetbulb_from_relhumid(
        Ftemp = temp_avg_f,
        RHvalue = relhum_avg_percent,
        MBpressure = station_pressure(
          elev_ft = elevation_ft,
          altimeter_setting = 1013.25,
          altimiter_setting_unit = "mb",
          output_unit = "mb"
        ),
        output_unit = "F"
      )
  )

# test scatterplot - "whole season"
subdat_scatter <- dat |>
  filter(month == 6 & hour_cst >= 600 & hour_cst <= 2100)

ggplot(
  subdat_scatter,
  aes(x = temp_avg_f, y = wind_spd_max_mph, color = deltat_f)
) +
  geom_point() +
  facet_wrap(~year)

# test linechart - "one week"
subdat_linewk <- dat |>
  filter(month == 6 & day <= 6 & year == 2021) |>
  mutate(
    timestamp_cst = str_c(year, month, day, sep = "-") |>
      str_c(hour_cst / 100, sep = " ") |>
      ymd_h(tz = "US/Central")
  )

subdat_lineavg <- dat |>
  filter(month == 6 & day <= 6 & year < 2021) |>
  group_by(month, day, hour_cst) |>
  mutate(wind_spd_max_mph = mean(wind_spd_max_mph)) |>
  mutate(deltat_f = mean(deltat_f)) |>
  distinct(month, day, hour_cst, wind_spd_max_mph, deltat_f) |>
  mutate(
    year = 2021,
    timestamp_cst = str_c(2021, month, day, sep = "-") |>
      str_c(hour_cst / 100, sep = " ") |>
      ymd_h(tz = "US/Central")
  )

ggplot(
  subdat_linewk,
  aes(
    x = timestamp_cst,
    y = wind_spd_max_mph,
    color = deltat_f
  )
) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 40)) +
  facet_wrap(~year)

ggplot(
  subdat_lineavg,
  aes(
    x = timestamp_cst,
    y = wind_spd_max_mph,
    color = deltat_f
  )
) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 40)) +
  facet_wrap(~year)
