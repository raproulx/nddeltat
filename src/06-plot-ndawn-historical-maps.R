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
  map(
    c("historical-both", "historical-delta-t"),
    \(x)
      data_in |>
        distinct(date) |>
        anti_join(
          tibble(
            date = dir_ls(
              switch(
                x,
                "historical-both" = {
                  "./results/maps_historical"
                },
                "historical-delta-t" = {
                  "./galleries"
                }
              ),
              type = "file",
              recurse = TRUE,
              regexp = x
            ) |>
              str_extract("\\d{4}-\\d{2}-\\d{2}") |>
              ymd()
          )
        ) |>
        pull(date)
  ) |>
  set_names(c("historical-both", "historical-delta-t"))

# create delta t and delta t + wind maps for each date, as needed ---------
plots_fun <- function(x) {
  dt <- list(
    ndawn = plot_weathermap(
      data_tibble = data_in,
      map_date = x,
      wth_variable = "delta_t",
      map_region = "ndawn",
      map_type = "historical",
      map_output = "image"
    ),
    mawn = plot_weathermap(
      data_tibble = data_in,
      map_date = x,
      wth_variable = "delta_t",
      map_region = "mawn",
      map_type = "historical",
      map_output = "image"
    ),
    all = plot_weathermap(
      data_tibble = data_in,
      map_date = x,
      wth_variable = "delta_t",
      map_region = "all",
      map_type = "historical",
      map_output = "image"
    )
  )

  if (x %in% (dates_needed |> pluck("historical-delta-t"))) {
    dt |>
      pluck("all") |>
      ggsave(
        filename = str_c("historical-delta-t-all-", x, ".png"),
        path = str_c(
          "./galleries/",
          format(x, "%b") |> str_to_lower(),
          "/images"
        ),
        width = 7.75,
        height = 5.66,
        units = "in",
        create.dir = TRUE
      )
  }

  if (x %in% (dates_needed |> pluck("historical-both"))) {
    ws <- plot_weathermap(
      data_tibble = data_in,
      map_date = x,
      wth_variable = "wind_speed",
      map_region = "ndawn",
      map_type = "historical",
      map_output = "image"
    )
    (dt |> pluck("ndawn") / ws) |>
      ggsave(
        filename = str_c("historical-both-ndawn-", x, ".png"),
        path = str_c(
          "./results/maps_historical/",
          format(x, "%b") |> str_to_lower()
        ),
        width = 7.75,
        height = 10.00,
        units = "in",
        create.dir = TRUE
      )

    ws <- plot_weathermap(
      data_tibble = data_in,
      map_date = x,
      wth_variable = "wind_speed",
      map_region = "mawn",
      map_type = "historical",
      map_output = "image"
    )
    (dt |> pluck("mawn") | ws) |>
      ggsave(
        filename = str_c("historical-both-mawn-", x, ".png"),
        path = str_c(
          "./results/maps_historical/",
          format(x, "%b") |> str_to_lower()
        ),
        width = 9.75,
        height = 5.25,
        units = "in",
        create.dir = TRUE
      )
  }
}

dates_needed |>
  list_c() |>
  unique() |>
  walk(
    \(x) plots_fun(x)
  )
