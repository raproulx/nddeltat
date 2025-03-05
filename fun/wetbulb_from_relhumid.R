# derived from NWS 'Dewpoint and Wet-bulb from Relative Humidity' calculator at https://www.weather.gov/epz/wxcalc_rh
wetbulb_from_relhumid <- function(
  Ftemp = NULL,
  Ctemp = NULL,
  RHvalue,
  MBpressure,
  output_unit = c("C", "F")
) {
  if (is.na(sum(Ftemp, Ctemp, RHvalue, MBpressure, na.rm = FALSE))) {
    return(as.numeric(NA))
  } else {
    if (is.null(Ctemp)) {
      Ctemp <- 0.55556 * (Ftemp - 32)
    }

    esubs <- function(Ctemp) {
      Es <- 6.112 * exp(17.67 * Ctemp / (Ctemp + 243.5))
      return(as.numeric(Es))
    }

    invertedRH <- function(Es, rh) {
      E <- Es * (rh / 100)
      return(as.numeric(E))
    }

    Es <- esubs(Ctemp)
    E2 <- invertedRH(Es, rh = RHvalue)

    Edifference <- 1
    Twguess <- 0
    previoussign <- 1
    incr <- 10

    while (abs(Edifference) > 0.005) {
      Ewguess <- 6.112 * exp((17.67 * Twguess) / (Twguess + 243.5))
      Eguess <- Ewguess -
        MBpressure * (Ctemp - Twguess) * 0.00066 * (1 + (0.00115 * Twguess))
      Edifference <- E2 - Eguess

      if (Edifference == 0) {
        incr <- 0
      } else {
        if (Edifference < 0) {
          cursign <- -1
          if (cursign != previoussign) {
            previoussign <- cursign
            incr <- incr / 10
          } else {
            incr <- incr
          }
        } else {
          cursign <- 1
          if (cursign != previoussign) {
            previoussign <- cursign
            incr <- incr / 10
          } else {
            incr <- incr
          }
        }
      }

      Twguess <- Twguess + incr * previoussign
    }

    wetbulb <- Twguess - incr * previoussign

    if (output_unit == "C") {
      return(round(as.numeric(wetbulb), 2))
    } else {
      return(round(as.numeric(wetbulb) * 1.8 + 32, 2))
    }
  }
}
