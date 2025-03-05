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
    convert_pressure <- function(val, unit_in, unit_out) {
      switch(
        unit_in,
        "inHg" = {
          switch(
            unit_out,
            "inHg" = {
              val
            },
            "mmHg" = {
              25.4 * val
            },
            "mb" = {
              33.8639 * val
            },
            "kPa" = {
              33.8639 * (val / 10)
            }
          )
        },
        "mmHg" = {
          switch(
            unit_out,
            "inHg" = {
              0.03937008 * val
            },
            "mmHg" = {
              val
            },
            "mb" = {
              1.333224 * val
            },
            "kPa" = {
              0.1333224 * val
            }
          )
        },
        "mb" = {
          switch(
            unit_out,
            "inHg" = {
              0.0295300 * val
            },
            "mmHg" = {
              0.750062 * val
            },
            "mb" = {
              val
            },
            "kPa" = {
              val / 10
            }
          )
        },
        "kPa" = {
          switch(
            unit_out,
            "inHg" = {
              0.295300 * val
            },
            "mmHg" = {
              7.50062 * val
            },
            "mb" = {
              10 * val
            },
            "kPa" = {
              val
            }
          )
        }
      )
    }

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
