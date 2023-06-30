#' @title convertACCMIP
#' @description function to convert ACCMIP data to isocountry resolution
#'
#' @param x MAgPIE object
#'
#' @return MAgPIE object
#'
#' @author Roman Popov, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("ACCMIP", subtype = "nhx_1850")
#' }
convertACCMIP <- function(x) {

  map <- toolGetMappingCoord2Country(pretty = TRUE)

  y <- toolAggregate(x, map, from = "coords", to = "iso", dim = 1)
  y <- toolCountryFill(y, fill = NA)

  return(y)
}
