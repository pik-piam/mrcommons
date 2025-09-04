#' @title calcRockNWeathering
#'
#' @description calculates amount of yearly N from rock weathering by country or global total,
#' disaggregated by land use type (6 classes from LanduseInitialisation)
#' @return MAgPIE object of amount of N (Mt)
#' @author David M Chen



calcRockNWeathering <- function() {
  x <- readSource("Houlton2018", convert = FALSE)

  # aggregate to country level
  x <- toolCoord2Isocell(x)

  landuse <- calcOutput("LanduseInitialisation", cellular = TRUE, nclasses = "six",
                        selectyears = "past", input_magpie = FALSE, aggregate = FALSE)
  landuseShr <- landuse / dimSums(landuse, dim = 3, na.rm = TRUE)

  # make past years - hold constant
  getYears(x) <- 1965
  x <- time_interpolate(x, interpolated_year = getYears(landuse), integrate_interpolated_years = TRUE)

  # split among landuse shares
  out <- x * landuseShr
  # aggregate
  out[is.na(out)] <- 0
  rel  <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mstools")
  out <- toolAggregate(out, rel = rel, from = "celliso", to = "iso")
  out <- toolCountryFill(out, fill = 0)
  return(list(x = out,
              weight = NULL,
              unit = "Mt N/year",
              description = "Amount of N weathered from rocks according to Houlton 2018",
              note = c("")))

}
