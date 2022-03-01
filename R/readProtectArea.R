#' @title readProtectArea
#' @description Read conservation priority areas (in Mha)
#' @return List of magpie objects with results on cellular level
#' @author David Chen, Felicitas Beier
#' @examples
#' \dontrun{
#' readSource("ProtectArea", convert = "onlycorrect")
#' }
#'
readProtectArea <- function() {

  x <- read.magpie("protect_area_0.5.mz")

  return(x)
}
