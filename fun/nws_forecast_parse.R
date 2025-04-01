# see: https://www.weather.gov/documentation/services-web-api
library(tidyverse)
nws_forecast_parse <- function(
  named_httr2_response = NULL,
  raw_variable = c(
    "temperature",
    "dewpoint",
    "maxTemperature",
    "minTemperature",
    "relativeHumidity",
    "apparentTemperature",
    "wetBulbGlobeTemperature",
    "heatIndex",
    "windChill",
    "skyCover",
    "windDirection",
    "windSpeed",
    "windGust",
    "weather",
    "hazards",
    "probabilityOfPrecipitation",
    "quantitativePrecipitation",
    "iceAccumulation",
    "snowfallAmount",
    "snowLevel",
    "ceilingHeight",
    "visibility",
    "transportWindSpeed",
    "transportWindDirection",
    "mixingHeight",
    "hainesIndex",
    "lightningActivityLevel",
    "twentyFootWindSpeed",
    "twentyFootWindDirection",
    "waveHeight",
    "wavePeriod",
    "waveDirection",
    "primarySwellHeight",
    "primarySwellDirection",
    "secondarySwellHeight",
    "secondarySwellDirection",
    "wavePeriod2",
    "windWaveHeight",
    "dispersionIndex",
    "pressure",
    "probabilityOfTropicalStormWinds",
    "probabilityOfHurricaneWinds",
    "potentialOf15mphWinds",
    "potentialOf25mphWinds",
    "potentialOf35mphWinds",
    "potentialOf45mphWinds",
    "potentialOf20mphWindGusts",
    "potentialOf30mphWindGusts",
    "potentialOf40mphWindGusts",
    "potentialOf50mphWindGusts",
    "potentialOf60mphWindGusts",
    "grasslandFireDangerIndex",
    "probabilityOfThunder",
    "davisStabilityIndex",
    "atmosphericDispersionIndex",
    "lowVisibilityOccurrenceRiskIndex",
    "stability",
    "redFlagThreatIndex"
  )
) {
  nws_obj <- named_httr2_response[[1]] |> resp_body_json()
  forecast_effective <- named_httr2_response[[1]] |>
    resp_header("Date") |>
    dmy_hms()
  forecast_expires <- named_httr2_response[[1]] |>
    resp_header("Expires") |>
    dmy_hms()
  location_elev_m <- nws_obj |> pluck("properties", "elevation", "value")
  location_id <- names(named_httr2_response)

  if (nws_obj |> pluck("properties", "forecastGenerator") |> is.null()) {
    if (length(raw_variable) > 1) {
      stop("Select one raw forecast variable to parse")
    } else {
      nws_obj |>
        pluck("properties", raw_variable) |>
        names() |>
        map(
          \(x)
            tibble(
              "{raw_variable}_{x}" := nws_obj |>
                pluck("properties", raw_variable, x)
            )
        ) |>
        bind_cols() |>
        unnest_wider(where(is.list), names_sep = "_") |>
        janitor::clean_names()
    }
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
    tibble(
      location_id,
      location_elev_m,
      forecast_effective,
      forecast_expires,
      periods
    ) |>
      relocate(
        c(forecast_type, forecast_expires, forecast_effective),
        .before = location_id
      )
  }
}
