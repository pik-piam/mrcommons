#' @title calcTemperature
#' @description calculates average monthly temperature on different landuse types
#'
#' @param landusetypes all or only one (to save computation memory)
#' @param months FALSE for yearly average, TRUE for monthly values
#' @param convert FALSE for raw values of temperature, TRUE add temperature of 15 degrees for countries
#' without observations or land mass.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#'
#' @examples
#' \dontrun{
#' calcOutput("Temperature")
#' }
#' @importFrom magpiesets findset addLocation


calcTemperature <- function(landusetypes = "all", months = FALSE, convert = TRUE) {

  temp <- calcOutput("LPJmLClimateInput_new", lpjmlVersion = "LPJmL4_for_MAgPIE_44ac93de",
                     climatetype = "GSWP3-W5E5:historical",
                     variable    = "temperature:monthlyMean",
                     stage       = "smoothed", aggregate = FALSE)

  if (!months) {
    temp <- dimSums(temp, dim = 3.1) / 12
  }

  landuse <- calcOutput("LanduseInitialisation", cellular = TRUE, cells = "lpjcell", aggregate = FALSE)
  landuse <- collapseDim(addLocation(landuse), dim = c("N", "cell"))
  landuse <- time_interpolate(landuse, interpolated_year = getYears(temp), extrapolation_type = "constant")

  # Aggregation
  landuseSum <- dimSums(landuse, dim = 3)
  outSum     <- toolAggregate(x = temp, weight = landuseSum, to = "iso")

  # using 15 degrees for countries without values
  missing <- unique(which(outSum == 0, arr.ind = TRUE)[, 1])
  outSum  <- outSum[-missing, , ]

  if (landusetypes != "all") {
    # Aggregation by landuse type
    landuse <- landuse[, , landusetypes]
    out <- toolAggregate(x = temp, weight = landuse, to = "iso")
    out <- out[-missing, , ]

    # put average temperature to landusetypes without measurements
    blank <- out * 0
    blank[out == 0] <- 1
    blank <- blank * outSum
    out <- out + blank
  } else {
    out <- outSum
  }

  if (convert) {
    out <- toolCountryFill(out, fill = 15)
  }

  weight <- calcOutput("LanduseInitialisation", cellular = FALSE, aggregate = FALSE)
  weight <- time_interpolate(weight, interpolated_year = getYears(out), extrapolation_type = "constant")
  if (landusetypes != "all") {
    weight <- weight[, , landusetypes]
  }

  return(list(x = out,
              weight = weight,
              unit = "Degree Celcius",
              min = -100,
              max = 100,
              description = "Countrylevel for Tmean",
              isocountries = convert))
}
