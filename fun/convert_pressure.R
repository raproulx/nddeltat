# derived from NWS 'Pressure Conversion' reference at https://www.weather.gov/media/epz/wxcalc/pressureConversion.pdf
# double-checked against Google unit converter
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
