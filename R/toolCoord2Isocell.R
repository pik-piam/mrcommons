#' @title       toolCoord2Isocell
#' @description Transforms an object with coordinate spatial data (on half-degree) to isocell (59199) standard
#'
#' @param x object to be transformed from coordinates to (old) magpie isocell standard
#'
#' @return magpie object with 59199 cells in isocell naming
#' @author Kristine Karstens, Felicitas Beier, Jan Philipp Dietrich
#'
#' @param cells Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @importFrom magpiesets addLocation
#' @importFrom madrat toolOrderCells
#' @importFrom magclass collapseDim
#'
#' @export

toolCoord2Isocell <- function(x, cells = "magpiecell") {

  if (cells == "magpiecell") {

    removedim <- setdiff(unlist(strsplit(names(getItems(x))[1], "\\.")), c("x", "y"))
    x <- collapseDim(x, dim = removedim)
    x <- addLocation(x)
    x <- collapseDim(x, dim = c("x", "y"))
    getItems(x, dim = 1.2)[getItems(x, dim = 1.2) == "0"] <- "NA"
    x <- toolOrderCells(x, na.rm = TRUE)
    if (length(getCells(x)) != 59199) warning("Some cells out of the 59199 standard cells are missing.")

  } else if (cells == "lpjcell") {

    getItems(x, dim = "cell",   maindim = 1) <- 1:67420
    x <- collapseDim(x, dim = c("x", "y"))

  } else {
stop("Unknown cells argument.")
}

  return(x)
}
