ndawn_retrieve <- function(
  stn_id,
  ndawn_fields,
  freq_type = "hourly",
  date_start = "yyyy-mm-dd",
  date_end = "yyyy-mm-dd"
) {
  library(tidyverse)
  library(httr2)
  library(suncalc)

  ndawn_stations <- read_csv("./data/tbl-ndawn-stations.csv")

  ndawn_definitions <-
    # fmt:skip
    tribble(
    ~ndawn_abbr, ~ndawn_field,              ~nws_field,
    "hdt",       "Avg Air Temp",            "temperature",
    "hdrh",      "Avg Rel Hum",             "relativeHumidity",
    "hdbst",     "Avg Bare Soil Temp",      "temperatureSoilBare",
    "hdtst",     "Avg Turf Soil Temp",      "temperatureSoilTurf",
    "hdws",      "Avg Wind Speed",          "windSpeed",
    "hdmxws",    "Max Wind Speed",          "windGust",
    "hdwd",      "Avg Wind Dir",            "windDirection",
    "hdsdwd",    "Avg Wind Dir SD",         "windDirectionStdDev",
    "hdsr",      "Avg Sol Rad",             "solarRadiation",
    "hdr",       "Total Rainfall",          "rainfall",
    "hdbp",      "Avg Baro Press",          "pressure", 
    "hddp",      "Avg Dew Point",           "dewpoint",   
    "hdwc",      "Avg Wind Chill",          "windChill",
    "hdt9",      "Avg Air Temp at 9 m",     "temperature9m", 
    "hdrh9",     "Avg Rel Hum at 9 m",      "relativeHumidity9m",
    "hdws10",    "Avg Wind Speed at 10 m",  "windSpeed10m",  
    "hdmxws10",  "Max Wind Speed at 10 m",  "windGust10m",
    "hdwd10",    "Avg Wind Dir at 10 m",    "windDirection10m", 
    "hdsdwd10",  "Avg Wind Dir SD at 10 m", "windDirectionStdDev10m"
  )

  url <- str_c(
    "https://ndawn.ndsu.nodak.edu/table.csv?station=",
    stn_id,
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
        with_tz(
          tzone = ndawn_stations |>
            dplyr::filter(station_id == stn_id) |>
            pull(timezone)
        ),
      station_id = stn_id
    ) |>
    relocate(station_id)

  out |>
    rename_with(
      ~ left_join(
        tibble(ndawn_field = ndawn_fields),
        ndawn_definitions
      ) |>
        pull(nws_field),
      .cols = all_of(ndawn_fields)
    ) |>
    rename_with(
      ~ left_join(
        tibble(ndawn_field = ndawn_fields),
        ndawn_definitions
      ) |>
        pull(nws_field) |>
        str_c(" Flag"),
      .cols = all_of(ndawn_fields |> str_c(" Flag"))
    ) |>
    relocate(StartingDatetime, .after = Elevation) |>
    select(-Year, -Month, -Day, -Hour) %>%
    bind_cols(
      getSunlightTimes(
        data = tibble(
          date = date(.$StartingDatetime),
          lat = .$Latitude,
          lon = .$Longitude
        ),
        keep = c("sunriseEnd", "sunsetStart"),
        tz = tz(.$StartingDatetime)
      ) |>
        select(sunriseEnd, sunsetStart)
    ) |>
    mutate(
      is_daytime = StartingDatetime >= sunriseEnd &
        (StartingDatetime + minutes(59) + seconds(59)) < sunsetStart
    ) |>
    select(-sunriseEnd, -sunsetStart) |>
    relocate(is_daytime, .after = StartingDatetime) |>
    janitor::clean_names()
}
