#' @title calcFertilizerPricesFAO
#' @description calculates dataset of fertilizer prices in US$MER05/tonne (either referring to the amount of fertilizer
#' product, or to the amount of nutrients within the fertilizer) based on FAO data
#' @param subtype "N" for fertilizer containing nitrogen, "P" for fertilizer containing phosphorus
#' @param by "nutrient" if referring to price per amount of nutrients (N or P) within the fertilizer products, or
#' "product" if referring to price per amount of fertilizer product
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("FertilizerPricesFAO", subtype = "N", by = "nutrient")
#' }
#' @importFrom GDPuc convertGDP

calcFertilizerPricesFAO <- function(subtype = "N", by = "nutrient") {

  ## read FAO data on fertilizer by product and subset to relevant products
  fertByProduct <- complete_magpie(readSource("FAO_online", "FertilizerProducts"), fill = 0)
  mapping <- toolGetMapping("fertilizer_products.csv", type = "sectoral")
  products <- mapping[mapping[, subtype] != "other", "product"]
  fertByProduct <- fertByProduct[, , products]

  ## fix dimension names and currency unit
  currencyDims <- c("import_kUS$", "export_kUS$")
  fertByProduc_currentUSD <- fertByProduct   # nolint
  fertByProduct[, , currencyDims] <- convertGDP(fertByProduc_currentUSD[, , currencyDims],
                                                unit_in = "current US$MER",
                                                unit_out = "constant 2005 US$MER",
                                                replace_NAs = "no_conversion") * 1000
  # for countries with missing conversion factors we assume no inflation:
  fertByProduct[is.na(fertByProduct)] <- fertByProduc_currentUSD[is.na(fertByProduct)]

  getNames(fertByProduct, dim = 2)[getNames(fertByProduct, dim = 2) == "import_kUS$"] <- "import_US$MER05"
  getNames(fertByProduct, dim = 2)[getNames(fertByProduct, dim = 2) == "export_kUS$"] <- "export_US$MER05"


  ## fertilizer price (per amount of fertilizer products)
  # calculate prices for import and export
  priceImport <- fertByProduct[, , "import_US$MER05", drop = TRUE] / fertByProduct[, , "import", drop = TRUE]
  priceExport <- fertByProduct[, , "export_US$MER05", drop = TRUE] / fertByProduct[, , "export", drop = TRUE]
  priceImport[!is.finite(priceImport)] <- 0
  priceExport[!is.finite(priceExport)] <- 0

  # choose lower price
  priceMin <- priceImport
  priceMin[priceMin == 0] <- priceExport[priceMin == 0]
  exportLower <- as.logical((priceImport > priceExport) * (priceExport != 0))
  priceMin[exportLower] <- priceExport[exportLower]
  priceMin[priceMin > 500] <- 0 # set unreasonably high values to missing

  # fill gaps with world averages per year and fertilizer item
  worldAvgPrice <- priceMin
  worldAvgPrice[worldAvgPrice != 0] <- 1
  worldAvgPrice[, , ] <- dimSums(priceMin, dim = 1) / dimSums(worldAvgPrice, dim = 1)
  worldAvgPrice[is.na(worldAvgPrice)] <- 0
  priceMin[priceMin == 0] <- worldAvgPrice[priceMin == 0]
  # before 51739 values missing, now 8217 (cases where no country reports a price)

  # calculate average fertilizer price over all products using agricultural use as weight
  weight <- fertByProduct[, , "Agricultural_Use_(tonnes)", drop = TRUE]
  weight <- weight / dimSums(weight, dim = 3)
  # assuming world-ratio between different fertilizer items for missing country weights
  worldRatio <- fertByProduct[, , "Agricultural_Use_(tonnes)", drop = TRUE]
  worldRatio[, , ] <- dimSums(worldRatio, dim = 1) / dimSums(worldRatio, dim = c(1, 3))
  weight[is.na(weight)] <- worldRatio[is.na(weight)]

  weight[priceMin[, , products] == 0] <- 0
  fertPrice <- toolAggregate(priceMin[, , products],
    rel = mapping[mapping[, subtype] != "other", ],
    weight = weight,
    from = "product", to = subtype, dim = 3.1
  )

  if (by == "product") {
    totalUseProducts <- calcOutput("FertilizerUseFAO", subtype = subtype, by = "product", aggregate = FALSE)
    res <- fertPrice
    weight <- totalUseProducts
    unit <- "US$MER05/tonne"
  } else if (by == "nutrient") {
    totalUseProducts <- calcOutput("FertilizerUseFAO", subtype = subtype, by = "product", aggregate = FALSE)
    totalUseNutrients <- calcOutput("FertilizerUseFAO", subtype = subtype, by = "nutrient", aggregate = FALSE)
    years <- intersect(getItems(totalUseNutrients, dim = 2), getItems(totalUseProducts, dim = 2))
    avgNutrientContent <- totalUseNutrients[, years, ] / totalUseProducts[, years, , drop = TRUE]
    res <- fertPrice[, years, , drop = TRUE] / avgNutrientContent
    res[!is.finite(res)] <- 0
    weight <- totalUseNutrients[, years, ]
    unit <- "US$MER05/tonne"
  }

  return(list(x = res,
              unit = unit,
              weight = weight,
              description = "fertilizer prices per amount of nutrients or amount of fertilizer products"))
}
