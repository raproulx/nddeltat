library(tidyverse)
library(httr2)

ndawn_retrieve <- function(
  station_id,
  ndawn_fields,
  freq_type = "hourly",
  date_start = yyyy - mm - dd,
  date_end = yyyy - mm - dd
) {
  ndawn_definitions <-
    # fmt:skip
    tribble(
    ~ndawn_abbr, ~ndawn_field, ~nws_field,
    "hdt",       "temperature",
    "hdrh",      "relativeHumidity",
    "hdbst",     "temperatureSoilBare",
    "hdtst",     "temperatureSoilTurf",
    "hdws",      "windSpeed",
    "hdmxws",    "windGust",
    "hdwd",      "windDirection",
    "hdsdwd",    "windDirectionStdDev",
    "hdsr",      "solarRadiation",
    "hdr",       "rainfall",
    "hdbp",      "pressure",
    "hddp",      "dewpoint",
    "hdwc",      "windChill",
    "hdt9",      "temperature9m",
    "hdrh9",     "relativeHumidity9m",
    "hdws10",    "windSpeed10m",
    "hdmxws10",  "windGust10m",
    "hdwd10",    "windDirection10m",
    "hdsdwd10",  "windDirectionStdDev10m"
  )
  "Avg Air Temp"
  "Avg Rel Hum"
  "Avg Bare Soil Temp"
  "Avg Turf Soil Temp"
  "Avg Wind Speed"
  "Max Wind Speed"
  "Avg Wind Dir"
  "Avg Wind Dir SD"
  "Avg Sol Rad"
  "Total Rainfall"
  "Avg Baro Press"
  "Avg Dew Point"
  "Avg Wind Chill"
  "Avg Air Temp at 9 m"
  "Avg Rel Hum at 9 m"
  "Avg Wind Speed at 10 m"
  "Max Wind Speed at 10 m"
  "Avg Wind Dir at 10 m"
  "Avg Wind Dir SD at 10 m"

  url <- str_c(
    "https://ndawn.ndsu.nodak.edu/table.csv?station=",
    station_id,
    "&variable=",
    str_c(
      ndawn_definitions |>
        dplyr::filter(ndawn_field %in% ndawn_fields) |>
        pull(ndawn_abbr),
      collapse = "&variable="
    ),
    "&ttype=",
    freq_type,
    "&quick_pick=&begin_date=",
    date_start,
    "&end_date=",
    date_end
  )

  resp <- url |>
    request() |>
    req_retry(
      max_tries = 5,
      is_transient = \(resp) resp_status(resp) %in% c(429, 500, 503, 504),
      backoff = \(resp) 10
    ) |>
    req_perform() |>
    resp_body_string()

  out <- resp |>
    read_csv(
      skip = 5,
      col_names = names(read_csv(resp, skip = 3, n_max = 0))
    ) |>
    mutate(
      StartingDatetime = ((str_c(
        str_c(Year, Month, Day, sep = "-"),
        " ",
        Hour / 100,
        ":00:00"
      ) |>
        ymd_hms()) +
        hours(6) -
        hours(1)) |>
        with_tz("US/Central")
    )

  out

  ##NEXT: finish tribble
  ##NEXT: replace NDAWN fields with NWS fields, then janitor::clean_names
  ##NEXT: decide if eliminating year, month, day, hour fields
}
