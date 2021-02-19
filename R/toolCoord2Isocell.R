#' @title       toolCoord2Isocell
#' @description Transforms an object with coordinate spatial data (on half-degree) to isocell (59199) standard
#' 
#' @param x object to be transformed from coordinates to (old) magpie isocell standard
#'
#' @return magpie object with 59199 cells in isocell naming
#' @author Kristine Karstens, Felicitas Beier
#' 
#' @importFrom magpiesets addLocation
#' @importFrom madrat toolOrderCells
#' @importFrom magclass collapseDim
#' 
#' @export

toolCoord2Isocell <- function(x) {
  
  # check if hasCoords works for more than x.y (e.g. x.y.iso) spatial dim names
  # if(!hasCoords(x)) stop()
  
  x <- collapseDim(x, keepdim = "x.y")
  x <- addLocation(x)
  x <- x["NA",,,invert=TRUE] 
  x <- toolOrderCells(x)
  if(length(getCells(x)) != 59199) stop("Some cells out of the 59199 standard cells are missing for this data set.
                                        Please first expand your object to cover all needed cells.")
  
  return(x)
}
