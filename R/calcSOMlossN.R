#' @title calcSOMlossN
#'
#' @description calculates effect on N  from Soil Organic Matter loss
#' @param cellular if TRUE cellular level is returned
#'
#' @return List of magpie object with results on country or cellular level,
#' weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky,
#' @examples
#' \dontrun{
#' calcOutput("SOMlossN")
#' }
#'
calcSOMlossN <- function(cellular = FALSE) {
  som <- calcOutput("SOM", aggregate = FALSE)
  som <- -som[, , "delta_soilc"][, , "cropland"] / 15

  if (!cellular) {
    som <- dimSums(som, dim = c("x", "y"))
    som <- toolCountryFill(som, fill = 0)
  }

  return(list(
    x = som,
    weight = NULL,
    unit = "Mt Nr",
    description = "Nitrogen release or bounding due to changes in Soil Organic Matter",
    isocountries = !cellular
  ))
}
