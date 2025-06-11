# load packages and functions ---------------------------------------------
library(tidyverse)
library(fs)
source("./fun/plot_weathermap.R")


# load plot data ----------------------------------------------------------
data_in <- read_csv("./data/tbl-ndawn-stations.csv") |>
  select(station_id, station_name, location, latitude, longitude) |>
  inner_join(
    read_csv(
      "./results/tbl-forecast-delta-t.csv",
      col_types = "cTTdcDTTccddcc"
    ),
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
        map_type = "forecast",
        map_output = "image"
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

# create interactive delta t map for today's date -------------------------
today_date <- (Sys.time() |> with_tz("America/Chicago") |> date())

plot_weathermap(
  data_tibble = data_in,
  map_date = today_date,
  wth_variable = "delta_t",
  map_type = "forecast",
  map_output = "leaflet_html"
) |>
  saveRDS(file = "./results/interactive-map-today-delta-t.rds")
