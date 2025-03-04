source("fun/wetbulb_from_relhumid.R")

dat <- readr::read_csv("data/00-carrington-test-data.csv") |>
  dplyr::rowwise() |>
  dplyr::mutate(
    deltat_f = temp_avg_f -
      wetbulb_from_relhumid(
        Ftemp = temp_avg_f,
        RHvalue = relhum_avg_percent,
        MBpressure = pressure_avg_mb,
        output_unit = "F"
      )
  )

#NOTE: used standard pressure for each station, adjusted to elevation, rather than measured atmospheric pressure
#NOTE: can perhaps write a function for this, using the same approach as NWS website (otherwise it's stored in stations csv file, which is a poor single source of truth)
