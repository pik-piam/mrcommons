#' @title correctHalfEarth
#' @description correct HalfEarth data
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author Felicitas Beier
#' @seealso
#'   \code{\link{readHalfEarth}}
#' @examples
#' \dontrun{
#' readSource("HalfEarth", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace
#' @importFrom magclass hasCoords

correctHalfEarth <- function(x) {

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)

  # cell mapping
  map         <- toolGetMappingCoord2Country()
  commonCells <- intersect(map$coords, getCells(x))

  y <- new.magpie(cells_and_regions = map$coords)
  y[commonCells, , ] <- x[commonCells, , ]

  y <- toolConditionalReplace(y, conditions = c("is.na()", "<0"), replaceby = 0)

  getSets(y)["d1.1"] <- "x"
  getSets(y)["d1.2"] <- "y"
  getCells(y)        <- paste(map$coords, map$iso, sep = ".")

  return(y)
}
