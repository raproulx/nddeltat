# load packages and functions ---------------------------------------------
library(tidyverse)
library(patchwork)
library(fs)
source("./fun/plot_weathermap.R")


# load plot data ----------------------------------------------------------
data_in <- read_csv("./data/tbl-ndawn-stations.csv") |>
  select(station_id, station_name) |>
  inner_join(
    read_csv(
      "./results/tbl-historical-delta-t.csv",
      col_types = "icddicdddcd"
    ) |>
      mutate(date = ymd(date)) |>
      select(-station_name),
    by = join_by(station_id == station_id)
  )

# determine dates that need mapping ---------------------------------------
dates_needed <-
  data_in |>
  distinct(date) |>
  anti_join(
    tibble(
      date = dir_ls(
        "./results/maps_historical",
        type = "file",
        recurse = TRUE
      ) |>
        str_extract("\\d{4}-\\d{2}-\\d{2}") |>
        ymd()
    )
  ) |>
  pull(date)

# create delta t + wind map for each date ---------------------------------
dates_needed |>
  map(
    \(x)
      (plot_weathermap(
        data_tibble = data_in,
        map_date = x,
        wth_variable = "delta_t",
        map_type = "historical"
      ) /
        plot_weathermap(
          data_tibble = data_in,
          map_date = x,
          wth_variable = "wind_speed",
          map_type = "historical"
        )) |>
        ggsave(
          filename = str_c("historical-both-", x, ".png"),
          path = str_c("./quarto-website/maps_historical/", format(x, "%b")),
          width = 7.75,
          height = 10.00,
          units = "in",
          create.dir = TRUE
        )
  )
