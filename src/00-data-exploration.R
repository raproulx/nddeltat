source("fun/wetbulb_from_relhumid.R")
source("fun/station_pressure.R")

dat <- readr::read_csv("data/00-carrington-test-data.csv") |>
  dplyr::rowwise() |>
  dplyr::mutate(
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
