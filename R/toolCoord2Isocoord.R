#' @title       toolCoord2Isocoord
#' @description Transforms an object with coordinate spatial data (on half-degree)
#'              to object with 67420 cells and coordinate and iso country information
#'
#' @param x     object to be transformed from coordinates to iso-coordinate object
#'
#' @return magpie object with 67420 cells in x.y.iso naming
#' @author Felicitas Beier
#'
#'
#' @export

toolCoord2Isocoord <- function(x) {

    # coordinate to country mapping for 67420 cells
    mapping <- toolGetMappingCoord2Country()
    mapping$coordiso <- paste(mapping$coords,
                              mapping$iso,
                              sep = ".")

    # sort first dimension as provided by mapping
    x <- x[mapping$coords, , ]
    # rename first dimension
    getItems(x, dim = 1, raw = TRUE) <- mapping$coordiso
    # set names
    getSets(x)["d1.1"] <- "x"
    getSets(x)["d1.2"] <- "y"
    getSets(x)["d1.3"] <- "iso"

  return(x)
}
