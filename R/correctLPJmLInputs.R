#' @title       correctLPJmLInputs
#' @description correct LPJmLInputs content (dummy function)
#'
#' @param x magpie object provided by the read function
#'
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("LPJmLInputs", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace
#'

correctLPJmLInputs <- function(x) {

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)

  return(x)
}
