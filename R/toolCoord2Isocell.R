#' @title       toolCoord2Isocell
#' @description Transforms an object with coordinate spatial data (on half-degree) to isocell (59199) standard
#' 
#' @param x object to be transformed from coordinates to (old) magpie isocell standard
#'
#' @return magpie object with 59199 cells in isocell naming
#' @author Kristine Karstens, Felicitas Beier
#' 
#' @param cells Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @importFrom magpiesets addLocation
#' @importFrom madrat toolOrderCells
#' @importFrom magclass collapseDim
#' 
#' @export

toolCoord2Isocell <- function(x, cells="magpiecell") {
  
  # check if hasCoords works for more than x.y (e.g. x.y.iso) spatial dim names
  # if(!hasCoords(x)) stop()
  
  if(cells=="magpiecell"){
    
    removedim <- setdiff(unlist(strsplit(names(getItems(x))[1],"\\.")), c("x","y"))
    x <- collapseDim(x, dim = removedim)
    x <- addLocation(x)
    x <- x["NA",,,invert=TRUE] 
    x <- collapseDim(x, dim=c("x","y"))
    x <- toolOrderCells(x)
    if(length(getCells(x)) != 59199) stop("Some cells out of the 59199 standard cells are missing for this data set.
                                        Please first expand your object to cover all needed cells.")
  } else if(cells=="lpjcell"){
    
    getItems(x, dim = "cell",   maindim = 1) <- 1:67420
    x <- collapseDim(x, dim = c("x","y"))
    
  } else {stop("Unknown cells argument.")}
  
  return(x)
}
