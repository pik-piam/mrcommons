#' @title calcSOMlossN

calcSOMlossN <- function(cellular = FALSE, cells = "lpjcell") {
  
  som <- calcOutput("SOM", cells = cells, aggregate = FALSE)
  som <- -som[, , "delta_soilc"][, , "cropland"] / 15

  if (!cellular) {

    # sum to iso-country level
    som <- dimSums(som, dim = c("x", "y"))
    som <- toolCountryFill(som, fill = 0)
  }

  return(list(
    x = som,
    weight = NULL,
    unit = "Mt Nr",
    description = "Nitrogen release or bounding due to changes in Soil Organic Matter",
    isocountries = !cellular))
}
