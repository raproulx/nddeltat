plot_weathermap <- function(
  data_tibble,
  map_date = "yyyy-mm-dd",
  wth_variable = c("delta_t", "wind_speed"),
  map_region = c("ndawn", "mawn", "all"),
  map_type = c("forecast", "historical"),
  map_output = c("image", "leaflet_html")
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
  library(leaflet)
  library(crosstalk)
  library(htmltools)
  source("./fun/windbarbs.R")

  map_date <- ymd(map_date)

  # visualization templates -------------------------------------------------
  counties <- list(
    ndawn = st_read("./data/geotemplate-counties-ndawn.geojson"),
    mawn = st_read("./data/geotemplate-counties-mawn.geojson"),
    all = st_read("./data/geotemplate-counties-all.geojson")
  )
  states <- list(
    ndawn = st_read("./data/geotemplate-states-ndawn.geojson"),
    mawn = st_read("./data/geotemplate-states-mawn.geojson"),
    all = st_read("./data/geotemplate-states-all.geojson")
  )
  bounding_box <- list(
    ndawn = st_read("./data/geotemplate-bounding-box-ndawn.geojson"),
    mawn = st_read("./data/geotemplate-bounding-box-mawn.geojson"),
    all = st_read("./data/geotemplate-bounding-box-all.geojson")
  )
  crs_gcs <- "epsg:4326"
  crs_pcs <- str_c("epsg:", st_crs(counties |> pluck(map_region))$epsg)

  scale_values <- list(
    delta_t = list(
      name = "\u00B0F",
      limits = c(0, 22),
      breaks = seq(0, 22, 2),
      labels = c(seq(0, 20, 2), "22+"),
      colorscale = tibble(
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
        values = c(0, 2.5, 4, 14, 15, 18, 19.5, 22, 40)
      ) # for future reference, a four-color colorblind-safe palette: "#117733", "#DDCC77", "#CC6677", "#882255"
    ),
    wind_speed = list(
      name = "mph",
      limits = c(0, 80),
      breaks = seq(0, 80, 10),
      labels = c(seq(0, 70, 10), "80+"),
      colorscale = tibble(
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
        values = c(0, 10, 20, 30, 40, 43, 57, 67, 80)
      )
    )
  )

  # subset plot data --------------------------------------------------------
  plotdat <- data_tibble |> dplyr::filter(date == map_date)

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
  r <- rast(bounding_box |> pluck(map_region), res = 1500)

  m <- fields::Tps(
    x = st_coordinates(plotdat),
    Y = plotdat |> pull(wth_variable)
  ) # thin plate spline model
  tps <- interpolate(r, m) |>
    mask(counties |> pluck(map_region))

  # clip plotdat to bounding box --------------------------------------------
  plotdat <- plotdat |>
    st_filter(counties |> pluck(map_region)) |>
    st_filter(bounding_box |> pluck(map_region))

  # map title, subtitle, and label ------------------------------------------
  map_title <- str_c(
    if (map_type == "forecast") {
      if (map_region == "mawn") {
        "<p style = 'font-size:17pt';><b> Always check the <img src = 'bin/ndawn-inversion-app-logo.png' height = 16> <span style = 'color:#3EB3FF;'> NDAWN Inversion</span><br>app for current conditions</b></p>\n"
      } else {
        "<p style = 'font-size:17pt';><b> Always check the <img src = 'bin/ndawn-inversion-app-logo.png' height = 16> <span style = 'color:#3EB3FF;'> NDAWN Inversion </span>app for current conditions</b></p>\n"
      }
    },
    if (map_region == "mawn") {
      "<p style = 'font-size:12pt';>"
    } else {
      "<p>"
    },
    switch(
      map_type,
      "forecast" = {
        "NWS Forecast "
      },
      "historical" = {
        "NDAWN "
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
    if (map_region != "mawn") {
      str_c(
        switch(
          map_type,
          "forecast" = {
            switch(
              wth_variable,
              "delta_t" = {
                str_c(
                  "<span style = 'color:white;'> ",
                  str_dup("-", 12),
                  " </span>"
                )
              },
              "wind_speed" = {
                str_c(
                  "<span style = 'color:white;'> ",
                  str_dup("-", 9),
                  " </span>"
                )
              }
            )
          },
          "historical" = {
            switch(
              wth_variable,
              "delta_t" = {
                str_c(
                  "<span style = 'color:white;'> ",
                  str_dup("-", 20),
                  " </span>"
                )
              },
              "wind_speed" = {
                str_c(
                  "<span style = 'color:white;'> ",
                  str_dup("-", 17),
                  " </span>"
                )
              }
            )
          }
        ),
        "<span style = 'color:black;font-family:Inconsolata;font-size:13pt'> ",
        plotdat |> pull(date) |> unique() |> format("%a, %b %d %Y")
      )
    },
    "</span></p>"
  )

  map_subtitle <-
    if (map_region == "mawn") {
      switch(
        map_type,
        "forecast" = {
          str_c(
            "Created: ",
            plotdat |>
              pull(forecast_effective) |>
              as_date() |>
              unique() |>
              format("%a, %b %d %Y"),
            switch(
              wth_variable,
              "delta_t" = {
                str_c(
                  "<span style = 'color:white;font-size:12.45pt'> ",
                  str_dup("-", 28),
                  " </span>"
                )
              },
              "wind_speed" = {
                str_c(
                  "<span style = 'color:white;'> ",
                  str_dup("-", 25),
                  " </span>"
                )
              }
            ),
            "<span style = 'color:black;font-family:Inconsolata;font-size:11pt'> ",
            plotdat |> pull(date) |> unique() |> format("%a, %b %d %Y"),
            "</span></p>"
          )
        },
        "historical" = {
          str_c(
            "<span style = 'color:black;font-family:Inconsolata;font-size:11pt'> ",
            plotdat |> pull(date) |> unique() |> format("%a, %b %d %Y"),
            "</span></p>"
          )
        }
      )
    } else {
      switch(
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
    }

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
  switch(
    map_output,
    "image" = {
      out <- ggplot() +
        labs(
          title = map_title,
          subtitle = map_subtitle
        ) +
        geom_sf(
          data = bounding_box |>
            pluck(map_region),
          color = "white",
          fill = "white"
        ) +
        geom_spatraster(
          data = tps
        ) +
        scale_fill_gradientn(
          name = scale_values |>
            pluck(wth_variable, "name"),
          na.value = "white",
          colors = scale_values |>
            pluck(wth_variable, "colorscale") |>
            pull(colors),
          values = scales::rescale(
            scale_values |>
              pluck(wth_variable, "colorscale") |>
              pull(values),
            from = scale_values |>
              pluck(wth_variable, "limits")
          ),
          oob = scales::oob_squish,
          limits = scale_values |>
            pluck(wth_variable, "limits"),
          breaks = scale_values |>
            pluck(wth_variable, "breaks"),
          labels = scale_values |>
            pluck(wth_variable, "labels")
        ) +
        geom_sf(
          data = counties |>
            pluck(map_region),
          color = "black",
          fill = NA
        ) +
        geom_sf(
          data = states |>
            pluck(map_region),
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
            length = 9,
            skip.x = 0,
            skip.y = 0,
            lwd = 0.8,
            fill = "gray35",
            colour = "gray35"
          )
      }

      nudge_imgcite <- switch(
        map_region,
        "ndawn" = {
          c(x = 185000, y = 45000, size = 0.55)
        },
        "mawn" = {
          c(x = 515000, y = 865000, size = 0.35)
        },
        "all" = {
          c(x = 265000, y = 200000, size = 0.55)
        }
      )

      nudge_txtcite <- switch(
        map_region,
        "ndawn" = {
          c(x = 388000, y = 0, size = 2.45)
        },
        "mawn" = {
          c(x = 355000, y = 750000, size = 1.33)
        },
        "all" = {
          c(x = 12500, y = 5000, size = 2.45)
        }
      )

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
            family = "Arial",
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
            x = st_bbox(bounding_box |> pluck(map_region))[1] +
              nudge_imgcite |> pluck("x"),
            y = st_bbox(bounding_box |> pluck(map_region))[2] +
              nudge_imgcite |> pluck("y")
          ),
          aes(x, y, image = "./bin/ndsu-extension-color-logo.eps"),
          size = nudge_imgcite |> pluck("size")
        ) +
        geom_richtext(
          data = tibble(
            x = st_bbox(bounding_box |> pluck(map_region))[1] +
              nudge_txtcite |> pluck("x"),
            y = st_bbox(bounding_box |> pluck(map_region))[2] +
              nudge_txtcite |> pluck("y")
          ),
          aes(
            x,
            y,
            family = "Arial",
            label = map_label
          ),
          size = nudge_txtcite |> pluck("size"),
          lineheight = 1.35,
          label.padding = unit(c(0, 0, 0, 0), "lines"),
          hjust = "left",
          vjust = "bottom",
          fill = NA,
          label.color = NA
        ) +
        coord_sf(
          xlim = st_bbox(bounding_box |> pluck(map_region))[c(1, 3)],
          ylim = st_bbox(bounding_box |> pluck(map_region))[c(2, 4)],
          crs = crs_pcs,
          expand = FALSE
        ) +
        theme(
          plot.title = element_markdown(family = "Arial"),
          plot.subtitle = element_markdown(family = "Arial"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(1, "null"),
          legend.frame = element_rect(color = "black"),
          legend.title.position = "right",
          legend.title = element_text(family = "Arial", vjust = 1, size = 14),
          legend.text = element_text(family = "Arial", size = 12),
          legend.ticks = element_line(color = "black"),
          legend.ticks.length = unit(c(-1, 0), 'mm')
        )

      out
      #girafe(ggobj = out)
    },
    "leaflet_html" = {
      # define raster colormap function
      switch(
        wth_variable,
        "delta_t" = {
          color_fun <- scales::pal_gradient_n(
            colours = scale_values |>
              pluck(wth_variable, "colorscale") |>
              slice_head(n = -1) |>
              pull(colors),
            values = scale_values |>
              pluck(wth_variable, "colorscale") |>
              slice_head(n = -1) |>
              pull(values)
          )
        },
        "wind_speed" = {
          color_fun <- scales::pal_gradient_n(
            colours = scale_values |>
              pluck(wth_variable, "colorscale") |>
              pull(colors),
            values = scale_values |>
              pluck(wth_variable, "colorscale") |>
              pull(values)
          )
        }
      )

      # create shared plotdata & leaflet map
      plotdat_sh <- SharedData$new(
        st_transform(plotdat, crs = 4326),
        ~station_name,
        group = "plot_tbl_pair"
      )

      lf_plot <- leaflet(plotdat_sh, elementId = "lf-plot") |>
        addTiles() |>
        addRasterImage(
          scales::oob_squish(
            tps,
            range = scale_values |> pluck(wth_variable, "limits")
          ),
          colors = color_fun,
          opacity = 0.55
        ) |>
        addPolygons(
          data = counties |>
            pluck(map_region) |>
            dplyr::select(geometry) |>
            st_simplify(dTolerance = 1000) |>
            st_intersection(bounding_box |> pluck(map_region)) |>
            st_transform(crs = 4326),
          color = "#343A40",
          weight = 1
        ) |>
        addLabelOnlyMarkers(
          data = plotdat |>
            st_transform(crs = 4326),
          label = ~ plotdat |>
            pull(wth_variable) |>
            round(digits = 0) |>
            as.character(),
          labelOptions = labelOptions(
            noHide = TRUE,
            textOnly = TRUE,
            textsize = "11px",
            style = list(
              color = "white",
              webkitTextStroke = "3px white"
            )
          )
        ) |>
        addLabelOnlyMarkers(
          data = plotdat_sh,
          label = ~ as.character(round(get(wth_variable), 0)),
          labelOptions = labelOptions(
            noHide = TRUE,
            textOnly = TRUE,
            textsize = "11px",
            style = list(
              color = "black"
            )
          )
        ) |>
        addMarkers(
          data = st_transform(plotdat, crs = 4326),
          group = "Station Labels",
          label = ~ htmlEscape(station_name),
          options = markerOptions(
            opacity = 0
          ),
          labelOptions = labelOptions(
            noHide = TRUE,
            offset = c(0, 25),
            textsize = "9px",
            style = list(
              backgroundColor = "#FFFFFF75",
              border = "none"
            )
          )
        ) |>
        addScaleBar(position = "bottomleft") |>
        hideGroup("Station Labels") |>
        addLayersControl(
          overlayGroups = c("Station Labels"),
          options = layersControlOptions(collapsed = FALSE)
        )

      lf_plot
    }
  )
}
