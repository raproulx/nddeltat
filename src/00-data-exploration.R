source("fun/wetbulb_from_relhumid.R")
source("fun/station_pressure.R")
library(tidyverse)

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
library(ggimage)
library(ggtext)

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
      dplyr::filter(date == "2025-05-03"),
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

# create smoothed raster
r <- rast(bounding_box, res = 1500)

m <- fields::Tps(x = st_coordinates(plotdat), Y = plotdat |> pull(delta_t)) # thin plate spline model
tps <- interpolate(r, m) |>
  mask(counties)

# create plot
out <- ggplot() +
  labs(
    title = str_c(
      "<p style = 'font-size:17pt';><b> Always check the <img src = 'bin/ndawn-inversion-app-logo.png' height = 16> <span style = 'color:#3EB3FF;'> NDAWN Inversion </span>app for current conditions</b></p>
      <p>NWS Forecast Maximum Delta-T (Wet Bulb Depression) (\u00B0F) <span style = 'color:white;'> \u2063\u2063\u2063\u2063\u2063\u2063 </span>",
      plotdat |> pull(date) |> unique() |> format("%a, %b %d %Y"),
      "</p>"
    ),
    subtitle = str_c(
      "Created: ",
      plotdat |>
        pull(forecast_effective) |>
        as_date() |>
        unique() |>
        format("%a, %b %d %Y")
    )
  ) +
  geom_sf(
    data = bounding_box,
    color = "white",
    fill = "white"
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
  geom_image(
    data = tibble(
      x = st_bbox(bounding_box)[1] + 185000,
      y = st_bbox(bounding_box)[2] + 45000
    ),
    aes(x, y, image = "./bin/ndsu-extension-color-logo.eps"),
    size = 0.55
  ) +
  geom_richtext(
    data = tibble(
      x = st_bbox(bounding_box)[1] + 388000,
      y = st_bbox(bounding_box)[2]
    ),
    aes(
      x,
      y,
      label = "Source: NOAA National Weather Service (NWS) Hourly Forecasts<br>
      https:&#47;&#47;www.ndsu.edu&#47;agriculture&#47;ag-home&#47;directory&#47;rob-proulx<br>
      Copyright (c) North Dakota State University<br>
      Background contouring does not necessarily reflect actual conditions"
    ),
    size = 2.55,
    lineheight = 1.35,
    label.padding = unit(c(0, 0, 0, 0), "lines"),
    hjust = "left",
    vjust = "bottom",
    fill = NA,
    label.color = NA
  ) +
  theme(
    plot.title = element_markdown(),
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

out
#girafe(ggobj = out)

# NWS forecast wind - ggplot map ---------------------------------------
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(ggrepel)
library(sf)
library(terra)
library(tidyterra)
library(rnaturalearth)
library(ggimage)
library(ggtext)
source("./fun/windbarbs.R")

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
      dplyr::filter(date == "2025-05-03"),
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

# create smoothed raster
r <- rast(bounding_box, res = 1500)

m <- fields::Tps(x = st_coordinates(plotdat), Y = plotdat |> pull(wind_speed)) # thin plate spline model
tps <- interpolate(r, m) |>
  mask(counties)

# create plot
out <- ggplot() +
  labs(
    title = str_c(
      "NWS Forecast Wind Speed (mph) at Time of Maximum Delta T <span style = 'color:white;'> \u2063\u2063\u2063\u2063\u2063\u2063 </span>",
      plotdat |> pull(date) |> unique() |> format("%a, %b %d %Y")
    ),
    subtitle = str_c(
      "Created: ",
      plotdat |>
        pull(forecast_effective) |>
        as_date() |>
        unique() |>
        format("%a, %b %d %Y")
    )
  ) +
  geom_sf(
    data = bounding_box,
    color = "white",
    fill = "white"
  ) +
  geom_spatraster(
    data = tps
  ) +
  scale_fill_gradientn(
    colors = c(
      "#FFFFFF",
      "#E8E88B",
      "#E6C52A",
      "#E68E06",
      "#F8521B",
      "#FD4027",
      "#8E3189",
      "#4A26BA",
      "#141444"
    ),
    na.value = "white",
    values = scales::rescale(
      c(0, 10, 20, 30, 40, 43, 57, 67, 80),
      from = c(0, 80)
    ),
    name = "mph",
    oob = scales::oob_squish,
    limits = c(0, 80),
    breaks = seq(0, 80, 10),
    labels = c(seq(0, 70, 10), "80+")
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
  geom_windbarb(
    data = plotdat,
    aes(
      x = pcs_longitude,
      y = pcs_latitude,
      mag = wind_speed,
      angle = wind_direction,
      mag.unit = wind_speed_unit
    ),
    length = 12,
    skip.x = 0,
    skip.y = 0,
    lwd = 0.8,
    fill = "gray35",
    colour = "gray35"
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
    aes(x = pcs_longitude, y = pcs_latitude, label = round(wind_speed, 0)),
    force = 0.0020,
    size = 2.8,
    box.padding = 0,
    bg.color = "white",
    bg.r = 0.15
  ) +
  geom_image(
    data = tibble(
      x = st_bbox(bounding_box)[1] + 185000,
      y = st_bbox(bounding_box)[2] + 45000
    ),
    aes(x, y, image = "./bin/ndsu-extension-color-logo.eps"),
    size = 0.55
  ) +
  geom_richtext(
    data = tibble(
      x = st_bbox(bounding_box)[1] + 388000,
      y = st_bbox(bounding_box)[2]
    ),
    aes(
      x,
      y,
      label = "Source: NOAA National Weather Service (NWS) Hourly Forecasts<br>
      https:&#47;&#47;www.ndsu.edu&#47;agriculture&#47;ag-home&#47;directory&#47;rob-proulx<br>
      Copyright (c) North Dakota State University<br>
      Background contouring does not necessarily reflect actual conditions"
    ),
    size = 2.55,
    lineheight = 1.35,
    label.padding = unit(c(0, 0, 0, 0), "lines"),
    hjust = "left",
    vjust = "bottom",
    fill = NA,
    label.color = NA
  ) +
  coord_sf(
    xlim = st_bbox(bounding_box)[c(1, 3)],
    ylim = st_bbox(bounding_box)[c(2, 4)],
    crs = crs_pcs,
    expand = FALSE
  ) +
  theme(
    plot.title = element_markdown(),
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

out
#girafe(ggobj = out)

#FIX: need to update R4.4.0 to get ggiraph to work in RStudio (otherwise, copy and paste code into R)
#NEXT: decide if it's worth including wind gust information (requires retrieving from raw forecast grid,
# interpolating values across times, and deciding under what circumstances gusts should be published);
# might make more sense to just work with raw forecast grid in that case, since already retrieving that info,
# with overlap joins in dplyr (https://dplyr.tidyverse.org/reference/join_by.html#overlap-joins) used
# to join sustained wind and wind gusts to a tibble of peak delta T
#NEXT: see how I inserted nozzles into AE1246 figures and see if that renders more clearly than ggimage
#NEXT: create a single function to create both graphs
#NEXT: write code to create map for each day of forecast period
#NEXT: figure out how to insert graphs into Quarto website, with proper scaling if using ggiraph
