library(tidyverse)
library(xml2)
library(rvest)


# define NDAWN URL --------------------------------------------------------
ndawn_url <- "https://ndawn.ndsu.nodak.edu/"


# read station info from NDAWN station map --------------------------------
ndawn_web_map <- read_html(ndawn_url)

ndawn_stations <- tibble(
  station_name = ndawn_web_map |>
    html_elements("area") |>
    html_attr("title"),
  station_id = ndawn_web_map |>
    html_elements("area") |>
    html_attr("href") |>
    str_extract("(?<=\\=).*"),
  station_url = ndawn_url |>
    str_c(
      ndawn_web_map |>
        html_elements("area") |>
        html_attr("href"),
      sep = ""
    )
)


# read details from each NDAWN station webpage ----------------------------
ndawn_details <- ndawn_stations |>
  pull(station_url) |>
  map(
    slowly(
      \(x)
        read_html(x) |>
          html_element("#details div") |>
          html_table() |>
          filter(X1 != "Details:") |>
          pivot_wider(names_from = X1, values_from = X2),
      rate = rate_delay(0.5)
    )
  ) |>
  bind_rows() |>
  janitor::clean_names()


# write NDAWN stations table to csv ----------------------------------------
ndawn_output <-
  bind_cols(
    ndawn_stations |> select(station_name, station_id),
    ndawn_details |>
      separate_wider_regex(
        elevation,
        c(elevation_ft = ".*", " feet \\(", elevation_m = ".*", " meters\\)")
      ) |>
      mutate(
        across(
          c(latitude, longitude),
          ~ str_remove(.x, pattern = "°") |> as.numeric()
        ),
        across(
          starts_with("elevation"),
          as.integer
        ),
        period_of_record = str_remove(period_of_record, " to .*") |> as_date(),
        timezone = lutz::tz_lookup_coords(
          lat = latitude,
          lon = longitude,
          method = "accurate"
        )
      ) |>
      rename(date_est = period_of_record)
  ) |>
  relocate(
    station_id,
    station_name,
    location,
    timezone,
    latitude,
    longitude,
    elevation_ft,
    elevation_m,
    date_est
  )

write_csv(ndawn_output, "./data/01-NDAWN-stations-table.csv")
