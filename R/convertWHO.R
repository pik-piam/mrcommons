#' @title convertWHO
#' @description Converts data from the WHO
#' @param x unconverted magpie object from read-script
#'
#' @return magpie object with a completed dataset.
#'
#' @seealso
#' [convertWHO()]


convertWHO <- function(x) {
  meanactivity <- dimSums(x, dim = 1) / nregions(x)
  x <- toolCountryFill(x, fill = NA)
  countries <- where(is.na(x))$true$regions
  x[countries, , ] <- meanactivity
  vcat(1, "better replace using function")
  return(x)
}
