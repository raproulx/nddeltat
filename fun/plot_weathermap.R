plot_weathermap <- function(
  map_date,
  wth_variable = c("delta_t", "wind_speed"),
  map_type = c("forecast", "historical")
) {
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

  # visualization templates -------------------------------------------------
  counties <- st_read("./data/geotemplate-counties.geojson")
  states <- st_read("./data/geotemplate-states.geojson")
  bounding_box <- st_read("./data/geotemplate-bounding-box.geojson")
  crs_gcs <- "epsg:4326"
  crs_pcs <- str_c("epsg:", st_crs(counties)$epsg)

  # define input datafile----------------------------------------------------
  data_infile <- switch(
    map_type,
    "forecast" = {
      "./results/tbl-forecast-delta-t.csv"
    },
    "historical" = {
      "./results/tbl-historical-delta-t.csv"
    }
  )

  # subset plot data --------------------------------------------------------
  plotdat <- read_csv("./data/tbl-ndawn-stations.csv") |>
    select(station_id, station_name, location, latitude, longitude) |>
    inner_join(
      read_csv(data_infile) |>
        dplyr::filter(date == map_date),
      by = join_by(station_id == location_id)
    )

  # convert plot data to sf object and reproject -----------------------------
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

  # create smoothed raster --------------------------------------------------
  r <- rast(bounding_box, res = 1500)

  m <- fields::Tps(
    x = st_coordinates(plotdat),
    Y = plotdat |> pull(wth_variable)
  ) # thin plate spline model
  tps <- interpolate(r, m) |>
    mask(counties)

  # map title, subtitle, and label ------------------------------------------
  map_title <- str_c(
    if (map_type == "forecast") {
      "<p style = 'font-size:17pt';><b> Always check the <img src = 'bin/ndawn-inversion-app-logo.png' height = 16> <span style = 'color:#3EB3FF;'> NDAWN Inversion </span>app for current conditions</b></p>\n"
    },
    switch(
      map_type,
      "forecast" = {
        "<p>NWS Forecast "
      },
      "historical" = {
        "<p>NDAWN "
      }
    ),
    switch(
      wth_variable,
      "delta_t" = {
        "Maximum Delta-T (Wet Bulb Depression) (\u00B0F) "
      },
      "wind_speed" = {
        "Wind Speed (mph) at Time of Maximum Delta T "
      }
    ),
    switch(
      map_type,
      "forecast" = {
        switch(
          wth_variable,
          "delta_t" = {
            "<span style = 'color:white;'> \u2063\u2063\u2063\u2063\u2063\u2063 </span>"
          },
          "wind_speed" = {
            "<span style = 'color:white;'> \u2063\u2063\u2003\u2063\u2002\u2063 </span>"
          }
        )
      },
      "historical" = {
        switch(
          wth_variable,
          "delta_t" = {
            "<span style = 'color:white;'> \u2063\u2063\u2063\u2063\u2063\u2063\u2063\u2063\u2063\u2063\u2002\u2063 </span>"
          },
          "wind_speed" = {
            "<span style = 'color:white;'> \u2063\u2063\u2063\u2063\u2063\u2063\u2063\u2063\u2063\u2063 </span>"
          }
        )
      }
    ),
    plotdat |> pull(date) |> unique() |> format("%a, %b %d %Y"),
    "</p>"
  )

  map_subtitle <- switch(
    map_type,
    "forecast" = {
      str_c(
        "Created: ",
        plotdat |>
          pull(forecast_effective) |>
          as_date() |>
          unique() |>
          format("%a, %b %d %Y")
      )
    },
    "historical" = {
      NULL
    }
  )

  map_label <- str_c(
    switch(
      map_type,
      "forecast" = {
        "Source: NOAA National Weather Service (NWS) Hourly Forecasts"
      },
      "historical" = {
        "Source: North Dakota Agricultural Weather Network (NDAWN) Hourly"
      }
    ),
    "<br>
     https:&#47;&#47;www.ndsu.edu&#47;agriculture&#47;ag-home&#47;directory&#47;rob-proulx<br>
     Copyright (c) North Dakota State University<br>
     Background contouring does not necessarily reflect actual conditions"
  )

  # create plot -------------------------------------------------------------
  out <- ggplot() +
    labs(
      title = map_title,
      subtitle = map_subtitle
    ) +
    geom_sf(
      data = bounding_box,
      color = "white",
      fill = "white"
    ) +
    geom_spatraster(
      data = tps
    ) +
    switch(
      wth_variable,
      "delta_t" = {
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
        )
      },
      "wind_speed" = {
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
        )
      }
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
    )

  if (wth_variable == "wind_speed") {
    out <- out +
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
      )
  }

  out <- out +
    geom_sf_interactive(
      fill = NA,
      color = "transparent",
      data = plotdat,
      aes(
        data_id = station_id,
        tooltip = glue::glue('{station_name}'),
        onclick = glue::glue(
          "window.open(\"https://forecast.weather.gov/MapClick.php?lat={gcs_latitude}&lon={gcs_longitude}\")"
        ),
        hover_css = "fill:transparent;stroke:transparent;r:5pt;"
      )
    ) +
    geom_text_repel(
      data = plotdat,
      aes(
        x = pcs_longitude,
        y = pcs_latitude,
        label = round(.data[[wth_variable]], 0)
      ),
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
        label = map_label
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
}
