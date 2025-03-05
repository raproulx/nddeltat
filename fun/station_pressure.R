# derived from NWS 'Station Pressure' calculator at https://www.weather.gov/epz/wxcalc_stationpressure
station_pressure <- function(
  elev_ft = NULL,
  elev_m = NULL,
  altimeter_setting = NULL,
  altimiter_setting_unit = c("inHg", "mmHg", "mb", "kPa"),
  output_unit = c("inHg", "mmHg", "mb", "kPa")
) {
  if (is.na(sum(elev_ft, elev_m, altimeter_setting, na.rm = FALSE))) {
    return(as.numeric(NA))
  } else {
    source("fun/convert_pressure.R")
    if (is.null(elev_m)) {
      elev_m <- 0.3048 * elev_ft
    }

    alt_inHg <- convert_pressure(
      val = altimeter_setting,
      unit_in = altimiter_setting_unit,
      unit_out = "inHg"
    )

    output <- alt_inHg * ((288 - 0.0065 * elev_m) / 288)^5.2561

    return(convert_pressure(output, "inHg", output_unit) |> round(2))
  }
}
