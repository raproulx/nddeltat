# source: https://www.weather.gov/documentation/services-web-api
library(tidyverse)
library(httr2)

nws_forecast_retrieve <- function(
  forecast_type = c("raw", "12h", "hourly"),
  latitude = NULL,
  longitude = NULL,
  output_id = NULL
) {
  api01 <- str_c("https://api.weather.gov/points/", latitude, ",", longitude)
  resp01 <- api01 |>
    request() |>
    req_retry(
      max_tries = 5,
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 503, 504),
      backoff = \(resp) 10
    ) |>
    req_perform() |>
    resp_body_json()

  resp02 <- switch(
    forecast_type,
    "raw" = {
      resp01$properties$forecastGridData
    },
    "12h" = {
      resp01$properties$forecast
    },
    "hourly" = {
      resp01$properties$forecastHourly
    }
  ) |>
    request() |>
    req_retry(
      max_tries = 5,
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 503, 504),
      backoff = \(resp) 10
    ) |>
    req_perform()

  if (is.null(output_id)) {
    output_id <- str_c(
      resp01$properties$relativeLocation$properties$city,
      resp01$properties$relativeLocation$properties$state,
      sep = ","
    )
  }

  list(timezone = resp01$properties$timeZone, resp = resp02) |>
    list() |>
    setNames(output_id)
}
