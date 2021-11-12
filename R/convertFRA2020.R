#' Convert FRA 2020 data
#'
#' @param x MAgPIE object containing original values
#' @param subtype The FAO FRA 2020 subtype.
#' @return Data as MAgPIE object with common country list
#' @author Abhijeet Mishra
#' @seealso [readSource()],
#' @examples
#' \dontrun{
#' a <- readSource("FRA2020", "growing_stock", convert = TRUE)
#' }
#' @importFrom madrat toolCountryFill
#'

convertFRA2020 <- function(x, subtype) {
  if (subtype %in% c("forest_area", "deforestation", "growing_stock", "management", "disturbance", "forest_fire")) {
    x <- toolCountryFill(x, fill = 0)

    if (any(getNames(x) %in% grep(pattern = "gs_ha", x = getNames(x), value = TRUE))) {
      ## This is done because gs_ha variables are already in m3/ha
      out <- x
    } else {
      out <- x / 1000 ## Conversion from 000 units to million units
    }
    return(out)
  } else if (subtype %in% c("biomass_stock", "carbon_stock")) {
    x <- toolCountryFill(x, fill = 0)
    out <- x
    return(out)
  } else {
    stop("Invalid subtype ", subtype)
  }
}
