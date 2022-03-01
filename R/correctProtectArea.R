#' @title correctProtectArea
#' @description Read calibrated protection area file
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author David Chen, Felicitas Beier
#' @seealso
#'   \code{\link{readProtectArea}}
#' @examples
#'
#' \dontrun{
#'   readSource("ProtectArea", convert="onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctProtectArea <- function(x) {

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
