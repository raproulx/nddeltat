# load packages and functions ---------------------------------------------
library(tidyverse)
library(sf)
library(xml2)
library(rvest)


# read NASS districts vector file -----------------------------------------
nass_dist <- st_read("./data/geotemplate-nass-districts.geojson")


# define NDAWN URL --------------------------------------------------------
ndawn_url <- "https://ndawn.ndsu.nodak.edu/"
ndawn_url_stn_all <- "https://ndawn.ndsu.nodak.edu/weather-data-daily.html"
ndawn_url_stn_active <- "https://ndawn.ndsu.nodak.edu/current.html"


# read station info -------------------------------------------------------
tbl_ndawn_station_all <- read_html(
  ndawn_url_stn_all
) |>
  html_elements("#table-stns") |>
  html_elements("option")

ndawn_stations_all <- tibble(
  station_name = tbl_ndawn_station_all |>
    html_text2() |>
    str_replace(", [A-Z]{2}", "") |>
    str_extract("^(.+?)(?=\\s\\()"),
  station_id = tbl_ndawn_station_all |>
    html_attr("value"),
  station_url = str_c(
    ndawn_url,
    "station-info.html?station=",
    station_id,
    sep = ""
  )
)

ndawn_stations_active <-
  read_html(ndawn_url_stn_active) |>
  html_element("#table") |>
  html_elements("tbody") |>
  html_table() |>
  pluck(1) |>
  rename(station_name = X1) |>
  select(station_name)

ndawn_stations <- ndawn_stations_all |> inner_join(ndawn_stations_active)

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


# format NDAWN stations table ---------------------------------------------
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
  )


# join NDAWN table to NASS districts and write to csv ---------------------
st_join(
  x = st_as_sf(
    ndawn_output,
    coords = c("longitude", "latitude"),
    remove = FALSE,
    crs = 4326
  ) |>
    st_transform(st_crs(nass_dist)),
  y = nass_dist
) |>
  janitor::clean_names() |>
  select(
    station_id,
    station_name,
    location,
    state,
    asd_no,
    asd_name,
    timezone,
    latitude,
    longitude,
    elevation_ft,
    elevation_m,
    date_est
  ) |>
  st_drop_geometry() |>
  write_csv("./data/tbl-ndawn-stations.csv")
