#' @title correctGAEZv4
#' @description Correct Global Agro-ecological Zones (GAEZ) data
#' @param x MAgPIE object provided by readGAEZv4 function
#' @return MAgPIE object at 0.5 cellular level
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("GAEZv4", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace
#' @importFrom magclass getYears getNames new.magpie mbind

correctGAEZv4 <- function(x) {

  mapping <- toolGetMappingCoord2Country(pretty = TRUE)
  tmp     <- new.magpie(cells_and_regions = setdiff(mapping$coords, getCells(x)),
                        years = getYears(x), names = getNames(x), fill = NA)

  x <- mbind(x, tmp)

  # NAs are set to 0
  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)

  # Sort cells correctly and rename
  x           <- x[mapping$coords, , ]
  getCells(x) <- paste(mapping$coords, mapping$iso, sep = ".")
  getSets(x)  <- c("x", "y", "iso", "year", "MCzones")

  return(x)
}
