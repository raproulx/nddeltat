# load packages and functions ---------------------------------------------
library(tidyverse) |> suppressMessages() |> suppressWarnings()
library(reactable) |> suppressMessages() |> suppressWarnings()
library(forcats) |> suppressMessages() |> suppressWarnings()

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

dt_col_def <- colDef(
  format = colFormat(digits = 1),
  minWidth = 54,
  style = function(value) {
    if (value <= 3.6) {
      color <- "#FFEA9E"
    } else if (value <= 14.4) {
      color <- "#88D488"
    } else if (value <= 18) {
      color <- "#FFEA9E"
    } else if (value <= 19.49) {
      color <- "#F4BA94"
    } else {
      color <- "#E88989"
    }
    list(background = color)
  }
)
# NEXT: group by NASS district
tbl_out <- reactable(
  tbl,
  height = 675,
  searchable = TRUE,
  groupBy = "asd_name",
  defaultExpanded = TRUE,
  paginateSubRows = TRUE,
  showPageSizeOptions = TRUE,
  pageSizeOptions = c(15, 30, 50, 100, nrow(tbl)),
  defaultPageSize = 15,
  wrap = FALSE,
  class = "dt-hr-tbl",
  elementId = "dt-hourly",
  defaultColDef = colDef(headerClass = "my-header"),
  style = reactableTheme(
    style = list(
      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
    )
  ),
  columns = list(
    station_name = colDef(
      name = "Station",
      sticky = "left",
      minWidth = 136
    ),
    asd_name = colDef(
      name = "Ag District",
      sticky = "left",
      minWidth = 100
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
