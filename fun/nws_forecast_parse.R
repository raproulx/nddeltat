# see: https://www.weather.gov/documentation/services-web-api
library(tidyverse)
nws_forecast_parse <- function(
  named_httr2_response = NULL
) {
  nws_obj <- named_httr2_response[[1]] |> resp_body_json()
  location_elev_m <- nws_obj |> pluck("properties", "elevation", "value")
  location_id <- names(named_httr2_response)

  if (nws_obj |> pluck("properties", "forecastGenerator") |> is.null()) {
    return("raw")
  } else {
    switch(
      nws_obj |> pluck("properties", "forecastGenerator"),
      "BaselineForecastGenerator" = {
        periods <- tibble(
          forecast_type = "12h",
          period = nws_obj |> pluck("properties", "periods")
        ) |>
          hoist(
            period,
            number = "number",
            name = "name",
            startTime = "startTime",
            endTime = "endTime",
            isDaytime = "isDaytime",
            temperature = "temperature",
            temperatureUnit = "temperatureUnit",
            temperatureTrend = "temperatureTrend",
            probabilityOfPrecipitation = "probabilityOfPrecipitation",
            windSpeed = "windSpeed",
            windDirection = "windDirection",
            shortForecast = "shortForecast",
            detailedForecast = "detailedForecast"
          ) |>
          unnest_wider(probabilityOfPrecipitation, names_sep = "_") |>
          mutate(
            startTime = ymd_hms(startTime) |> with_tz(),
            endTime = ymd_hms(endTime) |> with_tz()
          ) |>
          select(!period) |>
          janitor::clean_names()
      },
      "HourlyForecastGenerator" = {
        periods <- tibble(
          forecast_type = "hourly",
          period = nws_obj |> pluck("properties", "periods")
        ) |>
          hoist(
            period,
            number = "number",
            startTime = "startTime",
            endTime = "endTime",
            isDaytime = "isDaytime",
            temperature = "temperature",
            temperatureUnit = "temperatureUnit",
            temperatureTrend = "temperatureTrend",
            probabilityOfPrecipitation = "probabilityOfPrecipitation",
            dewpoint = "dewpoint",
            relativeHumidity = "relativeHumidity",
            windSpeed = "windSpeed",
            windDirection = "windDirection",
            shortForecast = "shortForecast"
          ) |>
          unnest_wider(
            c(probabilityOfPrecipitation, dewpoint, relativeHumidity),
            names_sep = "_"
          ) |>
          separate(windSpeed, c("windSpeed", "windSpeed_unit")) |>
          mutate(
            startTime = ymd_hms(startTime) |> with_tz(),
            endTime = ymd_hms(endTime) |> with_tz(),
            windSpeed = as.integer(windSpeed)
          ) |>
          select(!period) |>
          janitor::clean_names()
      }
    )
    tibble(location_id, location_elev_m, periods) |>
      relocate(forecast_type, .before = location_id)
  }
}
