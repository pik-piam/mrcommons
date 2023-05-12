#' @importFrom magclass getRegionList<- nregions
calcSOMlossN <- function(cellular = FALSE, cells = "magpiecell") {
  som <- calcOutput("SOM", cells = cells, aggregate = FALSE)
  som <- -som[, , "delta_soilc"][, , "cropland"] / 15

  if (!cellular) {

    mapping <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")
    som  <- toolAggregate(som, rel = mapping, from = ifelse(nregions(som) > 1, "celliso", "cell"), to = "iso", dim = 1)
    som  <- toolCountryFill(som, fill = 0)
  }

  return(list(
    x = som,
    weight = NULL,
    unit = "Mt Nr",
    description = "Nitrogen release or bounding due to changes in Soil Organic Matter",
    isocountries = !cellular))
}
