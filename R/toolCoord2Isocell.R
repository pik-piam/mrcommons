#' @title       toolCoord2Isocell
#' @description Transforms an object with coordinate spatial data (on half-degree)
#'              to isocell (59199) standard
#'
#' @param x     Object to be transformed from coordinates to (old) magpie isocell standard
#' @param cells Switch between "magpiecell" (59199) and "lpjcell" (67420)
#'
#' @return magpie object with 59199 cells in isocell naming
#' @author Kristine Karstens, Felicitas Beier, Jan Philipp Dietrich
#'
#' @param cells Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param fillMissing if NULL cells missing from the total 59199 are just being ignore. If set to a value
#' missing cells will be added with this value (e.g. all set to 0 if fillMissing is 0)
#' @param warnMissing Switch which controls whether missing cells should trigger a warning or not
#' @importFrom magpiesets addLocation
#' @importFrom madrat toolOrderCells
#' @importFrom magclass collapseDim
#' @importFrom utils packageVersion
#'
#' @export

toolCoord2Isocell <- function(x, cells = "magpiecell", fillMissing = NULL, warnMissing = TRUE) {
  if (cells == "magpiecell") {
    removedim <- setdiff(unlist(strsplit(names(getItems(x))[1], "\\.")), c("x", "y"))
    x <- collapseDim(x, dim = removedim)
    x <- addLocation(x, fillMissing = fillMissing, naCellNumber = "NA")
    x <- collapseDim(x, dim = c("x", "y"))
    x <- toolOrderCells(x, na.rm = TRUE)
    if (warnMissing && length(getCells(x)) != 59199) warning("Some cells out of the 59199 standard cells are missing.")
  } else if (cells == "lpjcell") {
    getItems(x, dim = "cell",   maindim = 1) <- 1:67420
    x <- collapseDim(x, dim = c("x", "y"))
  } else {
    stop("Unknown cells argument.")
  }
  return(x)
}
