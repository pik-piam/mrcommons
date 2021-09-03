#' @title convertILOSTAT
#' @description fills missing countries in ILOSTAT data with 0
#' @param x unconverted magpie object from read-script
#' @param subtype Type of ILOSTAT data that should be read
#' @return Data as MAgPIE object with common country list
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   a <- readSource("ILOSTAT", "EmplByActivityModelled", convert = TRUE)
#' }

convertILOSTAT <- function(x, subtype) {
  x <- toolCountryFill(x, fill = 0)
  x[!is.finite(x)] <- 0
  return(x)
}
