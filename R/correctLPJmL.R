#' @title correctLPJmL
#' @description Correct LPJmL content
#'
#' @param x magpie object provided by the read function
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens, Felicitas Beier
#' @seealso
#' [correctLPJmL()]
#'
#' @examples
#' \dontrun{
#' readSource("LPJmL", subtype = "soilc", convert = "onlycorrect")
#' }
#'
#' @importFrom lpjclass readLPJ

correctLPJmL <- function(x) {

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)
  if (length(getCells(x)) == 59199) {
    x <- toolCell2isoCell(x)
  }

  return(x)
}
