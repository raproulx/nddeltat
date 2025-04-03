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


# NWS forecast delta T - ggplot map ---------------------------------------
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(ggrepel)
library(sf)
library(terra)
library(tidyterra)
library(rnaturalearth)
library(RColorBrewer)

# visualization templates
counties <- st_read("./data/geotemplate-counties.geojson")
states <- st_read("./data/geotemplate-states.geojson")
bounding_box <- st_read("./data/geotemplate-bounding-box.geojson")

crs_gcs <- "epsg:4326"
crs_pcs <- str_c("epsg:", st_crs(counties)$epsg)

# subset plot data
plotdat <- read_csv("./data/tbl-ndawn-stations.csv") |>
  select(station_id, station_name, location, latitude, longitude) |>
  inner_join(
    read_csv("./results/tbl-forecast-delta-t.csv") |>
      dplyr::filter(date == "2025-04-07"),
    by = join_by(station_id == location_id)
  )

# convert plotdata to sf object and reproject
plotdat <- plotdat |>
  st_as_sf(coords = c("longitude", "latitude"), crs = crs_gcs) |>
  st_transform(crs = crs_pcs)

plotdat <- plotdat |>
  bind_cols(
    plotdat |>
      st_transform(crs_gcs) |>
      st_coordinates() |>
      as_tibble() |>
      rename(gcs_latitude = Y, gcs_longitude = X)
  ) |>
  bind_cols(
    plotdat |>
      st_coordinates() |>
      as_tibble() |>
      rename(pcs_latitude = Y, pcs_longitude = X)
  )

#TEST plotdata with delta T beyond range
#plotdat <- plotdat |>
#  mutate(delta_t = if_else(delta_t >= 12, delta_t + 10, delta_t + 5))

# create smoothed raster
r <- rast(bounding_box, res = 1500)

m <- fields::Tps(x = st_coordinates(plotdat), Y = plotdat |> pull(delta_t)) # thin plate spline model
tps <- interpolate(r, m) |>
  mask(counties)

# create plot
out <- ggplot() +
  labs(
    title = str_c(
      "NWS Forecast Peak Hourly Delta-T (Wet Bulb Depression) for ",
      plotdat |> pull(date) |> unique() |> format("%a, %b %d %Y"),
      " (\u00B0F)"
    ),
    subtitle = str_c(
      "Created: ",
      plotdat |>
        pull(forecast_effective) |>
        as_date() |>
        unique() |>
        format("%a, %b %d %Y")
    ),
    caption = "Source: NOAA National Weather Service (NWS) Hourly Forecasts"
  ) +
  geom_spatraster(
    data = tps
  ) +
  scale_fill_gradientn(
    name = "\u00B0F",
    colors = c(
      "#FFFF00",
      "#FFFF00",
      "#008000",
      "#008000",
      "#FFFF00",
      "#FFFF00",
      "#FF0000",
      "#FF0000",
      "#FF0000"
    ),
    na.value = "white",
    values = scales::rescale(
      c(0, 2.5, 4, 14, 15, 18, 19.5, 22, 40),
      from = c(0, 22)
    ),
    oob = scales::oob_squish,
    limits = c(0, 22),
    breaks = seq(0, 22, 2),
    labels = c(seq(0, 20, 2), "22+")
  ) +
  geom_sf(
    data = counties,
    color = "black",
    fill = NA
  ) +
  geom_sf(
    data = states,
    color = "black",
    linewidth = 1,
    fill = NA
  ) +
  geom_sf_interactive(
    fill = NA,
    color = "transparent",
    data = plotdat,
    aes(
      data_id = station_id,
      tooltip = glue::glue('{station_name}'),
      #FIX: pull lat and long from code
      onclick = glue::glue(
        "window.open(\"https://forecast.weather.gov/MapClick.php?lat={gcs_latitude}&lon={gcs_longitude}\")"
      ),
      hover_css = "fill:transparent;stroke:transparent;r:5pt;"
    )
  ) +
  geom_text_repel(
    data = plotdat,
    aes(x = pcs_longitude, y = pcs_latitude, label = round(delta_t, 0)),
    force = 0.0020,
    size = 2.8,
    box.padding = 0,
    bg.color = "white",
    bg.r = 0.15
  ) +
  coord_sf(
    xlim = st_bbox(bounding_box)[c(1, 3)],
    ylim = st_bbox(bounding_box)[c(2, 4)],
    crs = crs_pcs,
    expand = FALSE
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, "null"),
    legend.frame = element_rect(color = "black"),
    legend.title.position = "right",
    legend.title = element_text(vjust = 1, size = 14),
    legend.text = element_text(size = 12),
    legend.ticks = element_line(color = "black"),
    legend.ticks.length = unit(c(-1, 0), 'mm')
  )

#out
girafe(ggobj = out)

#FIX: need to update R4.4.0 to get ggiraph to work in RStudio (otherwise, copy and paste code into R)
#FIX: see why thin border along top (write raster to file, show in QGIS with bounding box over top - likely need to make bounding box bigger)
#THEN: try make a wind map
#THEN: add text and NDSU Extension logo over the top of SD space
