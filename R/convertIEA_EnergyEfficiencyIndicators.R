#' Convert IEA End Uses and Efficiency Indicators data to data on ISO country level.
#'
#' @author Falk Benke
#' @param x MAgPIE object to be converted
#' @importFrom madrat toolCountry2isocode toolCountryFill
convertIEA_EnergyEfficiencyIndicators <- function(x) { #nolint object_name_linter
  x <- x["IEATOT", , , invert = TRUE]

  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1), warn = TRUE)

  # aggregate Kosovo to Serbia
  x1 <- x["KOS", , ]
  getItems(x1, dim = 1) <- c("SRB")
  x["SRB", , ] <- x["SRB", , ] + x1
  x <- x[c("KOS"), , , invert = TRUE]

  x <- toolCountryFill(x, 0, verbosity = 2)

  return(x)
}
