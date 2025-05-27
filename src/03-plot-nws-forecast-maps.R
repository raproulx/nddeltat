# load packages and functions ---------------------------------------------
library(tidyverse)
library(fs)
source("./fun/plot_weathermap.R")


# load plot data ----------------------------------------------------------
data_in <- read_csv("./data/tbl-ndawn-stations.csv") |>
  select(station_id, station_name, location, latitude, longitude) |>
  inner_join(
    read_csv("./results/tbl-forecast-delta-t.csv", col_types = "cTTdDTTddcc"),
    by = join_by(station_id == location_id)
  )

# create delta t map for each date ----------------------------------------
data_in |>
  pull(date) |>
  unique() |>
  walk(
    \(x)
      plot_weathermap(
        data_tibble = data_in,
        map_date = x,
        wth_variable = "delta_t",
        map_type = "forecast"
      ) |>
        ggsave(
          filename = str_c("forecast-delta-t-", x, ".png"),
          path = str_c(
            "./results/maps_forecast/",
            str_to_lower(format(x, "%b"))
          ),
          width = 7.75,
          height = 5.66,
          units = "in",
          create.dir = TRUE
        )
  )
