#' @title correctSoilGrids
#' @description Correct SoilGrids content
#' @param x magpie object provided by the read function
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' [readSoilGrids()]
#' @examples
#' \dontrun{
#' readSource("SoilGrids", subtype = "cstock_0_30", convert = "onlycorrect")
#' }
correctSoilGrids <- function(x) {

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)

  return(x)
}
