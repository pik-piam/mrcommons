#' @title correctTNC2019
#' @description correct biome data
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author Patrick v. Jeetze
#' @seealso
#'   \code{\link{readTNC2019}}
#' @examples
#' \dontrun{
#' readSource("TNC2019", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctTNC2019 <- function(x) {
  x <- toolConditionalReplace(x, conditions = "is.na()", replaceby = 0)

  return(x)
}
