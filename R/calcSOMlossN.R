#' @title calcSOMlossN
#'
#' @description calculates effect on N  from Soil Organic Matter loss
#' @param cellular if TRUE cellular level is returned
#' @param cells    "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#'
#' @return List of magpie object with results on country or cellular level,
#' weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky,
#' @examples
#' \dontrun{
#' calcOutput("SOMlossN")
#' }
#'

calcSOMlossN <- function(cellular = FALSE, cells = "lpjcell") {

  som <- calcOutput("SOM", cells = cells, aggregate = FALSE)
  som <- -som[, , "delta_soilc"][, , "cropland"] / 15

  if (!cellular) {

    # sum to iso-country level
    som <- toolConv2CountryByCelltype(som, cells = cells)
    som <- toolCountryFill(som, fill = 0)
  }

  return(list(
    x = som,
    weight = NULL,
    unit = "Mt Nr",
    description = "Nitrogen release or bounding due to changes in Soil Organic Matter",
    isocountries = !cellular))
}
