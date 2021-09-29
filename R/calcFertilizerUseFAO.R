#' @title calcFertilizerUseFAO
#' @description calculates dataset of fertilizer use in tonnes (either referring to the amount of fertilizer products
#' used, or to the amount of nutrients within the fertilizer used) based on FAO data
#' @param subtype "N" for fertilizer containing nitrogen, "P" for fertilizer containing phosphorus (note that there
#' is an overlap between those categories, as some fertilizers include both nutrients)
#' @param by "nutrient" if referring to amount of nutrients (N or P) in total used fertilizer, or "product" if
#' referring to total amount of fertilizer used
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("FertilizerUseFAO", subtype = "N", by = "nutrient")
#' }
#'
calcFertilizerUseFAO <- function(subtype = "N", by = "nutrient") {

  ## read FAO data on fertilizer by nutrient
  if (subtype == "N") {
    nutrient <- "3102|Nutrient nitrogen N (total)"
  } else if (subtype == "P") {
    nutrient <- "3103|Nutrient phosphate P2O5 (total)"
  }
  fertByNutrient <- readSource("FAO_online", "FertilizerNutrients")[, , nutrient, drop = TRUE]
  totalUseNutrients <- setNames(fertByNutrient[, , "Agricultural_Use_(tonnes)", drop = TRUE], subtype)

  # calculate nutrients per area (to fill in fertilizer by nutrient data later on)
  cropland <- readSource("FAO_online", "Land")[, , list("6620|Cropland", "Area_(1000_ha)"), drop = TRUE]
  nutrientPerArea <- totalUseNutrients / cropland
  nutrientPerArea[is.na(nutrientPerArea)] <- 0

  # fill gaps in nutrients per area data by using regional averages (scaled by a mulitplicative correction coefficient
  # to match first observation of each country)
  regionMapping <- toolGetMapping("regionmappingH12.csv", type = "regional")
  numCountries <- nutrientPerArea
  numCountries[numCountries != 0] <- 1
  numCountries <- toolAggregate(numCountries, rel = regionMapping, weight = NULL,
                                from = "CountryCode", to = "RegionCode", dim = 1)
  worldRegionNutrientPerArea <- toolAggregate(nutrientPerArea, rel = regionMapping,
                                        weight = NULL, from = "CountryCode", to = "RegionCode", dim = 1) / numCountries

  .calibratingWorldRegionValues <- function(reg, x) {
    tmp <- x[reg, , ]
    wr <- regionMapping$RegionCode[regionMapping$CountryCode == reg]
    yearsObs <- where(tmp != 0)$true$years
    yearsMissing <- where(tmp == 0)$true$years
    if (length(yearsObs) == 0) {
      tmp[, , ] <- worldRegionNutrientPerArea[wr, , drop = TRUE]
    } else if (length(yearsMissing) > 0) {
      y <- min(yearsObs)
      factor <- tmp[, y, ] / worldRegionNutrientPerArea[wr, y, , drop = TRUE]
      tmp[, yearsMissing, ] <- worldRegionNutrientPerArea[wr, yearsMissing, , drop = TRUE] * factor
    }
    return(tmp)
  }
  nutrientPerArea <- mbind(lapply(getItems(nutrientPerArea, dim = 1), .calibratingWorldRegionValues, nutrientPerArea))

  # fill gaps in fertilizer use by nutrient based on cropland and nutrient per area
  estFertByNutrient <- cropland * nutrientPerArea
  totalUseNutrients[totalUseNutrients == 0] <- estFertByNutrient[totalUseNutrients == 0]


  ## read FAO data on fertilizer by product and subset to fertilizer products of given nutrient type
  fertByProduct <- complete_magpie(readSource("FAO_online", "FertilizerProducts"), fill = 0)
  mapping <- toolGetMapping("fertilizer_products.csv", type = "sectoral")
  products <- mapping[mapping[, subtype] != "other", "product"]
  fertByProduct <- fertByProduct[, , products]
  totalUseProducts <- setNames(dimSums(fertByProduct[, , "Agricultural_Use_(tonnes)"], dim = 3.1),
                               paste0(subtype, "_fertilizer"))

  ## average nutrient content in fertilizer products
  avgNutrientContent <- totalUseNutrients[, getItems(totalUseProducts, dim = 2), , drop = TRUE] / totalUseProducts
  avgNutrientContent[avgNutrientContent > 1] <- 0
  avgNutrientContent[!is.finite(avgNutrientContent)] <- 0

  # fill gaps with world averages
  worldAvgNutrientContent <- avgNutrientContent
  worldAvgNutrientContent[worldAvgNutrientContent != 0] <- 1
  worldAvgNutrientContent[, , ] <- dimSums(avgNutrientContent, dim = 1) / dimSums(worldAvgNutrientContent, dim = 1)
  worldAvgNutrientContent[is.na(worldAvgNutrientContent)] <- 0
  avgNutrientContent[avgNutrientContent == 0] <- worldAvgNutrientContent[avgNutrientContent == 0]

  ## fill gaps in total fertilizer use based on fertilizer by nutrient use
  estTotalUseProducts <- totalUseNutrients[, getItems(totalUseProducts, dim = 2), , drop = TRUE] / avgNutrientContent
  totalUseProducts[totalUseProducts == 0] <- estTotalUseProducts[totalUseProducts == 0]

  ## select output
  if (by == "nutrient") {
    res <- totalUseNutrients
    weight <- NULL
    unit <- "tonnes"
  } else if (by == "product") {
    res <- totalUseProducts
    weight <- NULL
    unit <- "tonnes"
  }

  getSets(res) <- c("region", "year", by)

  return(list(x = res,
              unit = unit,
              weight = weight,
              description = "total use of fertilizer either in amount of nutrients or amount of fertilizer products"))
}
