#' @title correctLanduseToolbox
#' @description correct Landuse Toolbox output data. Convert unit from ha to mio ha
#' @return corrected magpie object
#' @param x magpie object provided by the read function
#' @author David Hoetten
#' @seealso
#'   \code{\link{readLanduseToolbox}}
#' @examples
#' \dontrun{
#' A <- readSource("LanduseToolbox", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctLanduseToolbox <- function(x) {
  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)
  x <- x * 10^-6 # convert from ha to mio ha
  return(x)
}
