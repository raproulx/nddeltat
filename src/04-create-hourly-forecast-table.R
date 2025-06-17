# load packages and functions ---------------------------------------------
library(tidyverse)
library(reactable)
library(crosstalk)
library(forcats)

# read in data ------------------------------------------------------------
data_in <- read_csv("./data/tbl-ndawn-stations.csv", show_col_types = FALSE) |>
  select(station_id, station_name, state, asd_no, asd_name) |>
  inner_join(
    read_csv(
      "./results/tbl-forecast-delta-t-hourly.csv",
      col_types = "cTTdcDTTccddcc",
      show_col_types = FALSE
    ),
    by = join_by(station_id == location_id)
  ) |>
  mutate(
    state = fct(
      state,
      levels = c("Montana", "North Dakota", "Minnesota")
    )
  ) |>
  arrange(state, asd_no, station_name)


# create reactable table --------------------------------------------------
tbl <- data_in |>
  dplyr::filter(date == (Sys.time() |> with_tz("America/Chicago") |> date())) |>
  select(station_name, asd_name, start_time_local, delta_t) |>
  pivot_wider(names_from = start_time_local, values_from = delta_t)

tbl_sh <- SharedData$new(
  tbl,
  ~station_name,
  group = "plot_tbl_pair"
)

dt_col_def <- colDef(
  format = colFormat(digits = 1),
  minWidth = 62,
  style = function(value) {
    color_fun <- scales::pal_gradient_n(
      colours = c(
        "#FFEA9E",
        "#FFEA9E",
        "#88D488",
        "#88D488",
        "#FFEA9E",
        "#FFEA9E",
        "#E88989",
        "#E88989"
      ),
      values = c(0, 2.5, 4, 14, 15, 18, 19.5, 22)
    )

    color <- color_fun(scales::oob_squish(value, range = c(0, 22)))

    list(background = color)
  }
)

tbl_out <- reactable(
  tbl_sh,
  #height = 675,
  searchable = FALSE,
  groupBy = "asd_name",
  defaultExpanded = TRUE,
  paginateSubRows = TRUE,
  showPageSizeOptions = TRUE,
  pageSizeOptions = c(
    15,
    30,
    50,
    100,
    nrow(tbl) + nrow(distinct(tbl, asd_name))
  ),
  defaultPageSize = nrow(tbl) + nrow(distinct(tbl, asd_name)),
  wrap = FALSE,
  class = "dt-tbl",
  elementId = "dt-hourly",
  defaultColDef = colDef(class = "dt-tbl-body", headerClass = "dt-tbl-head"),
  style = reactableTheme(
    style = list(
      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
    )
  ),
  columns = list(
    station_name = colDef(
      name = "Station",
      sticky = "left",
      minWidth = 152
    ),
    asd_name = colDef(
      name = "Ag District",
      sticky = "left",
      minWidth = 126
    ),
    `6 AM` = dt_col_def,
    `7 AM` = dt_col_def,
    `8 AM` = dt_col_def,
    `9 AM` = dt_col_def,
    `10 AM` = dt_col_def,
    `11 AM` = dt_col_def,
    `12 PM` = dt_col_def,
    `1 PM` = dt_col_def,
    `2 PM` = dt_col_def,
    `3 PM` = dt_col_def,
    `4 PM` = dt_col_def,
    `5 PM` = dt_col_def
  )
)

saveRDS(tbl_out, file = "./results/interactive-tbl-today-delta-t.rds")
