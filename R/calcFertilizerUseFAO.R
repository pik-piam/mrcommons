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
  years <- intersect(getItems(cropland, dim = 2), getItems(totalUseNutrients, dim = 2))
  totalUseNutrients <- totalUseNutrients[, years, ]
  cropland <- cropland[, years, ]
  nutrientPerArea <- totalUseNutrients / cropland
  nutrientPerArea[!is.finite(nutrientPerArea)] <- 0

  # fill gaps in nutrients per area data by using regional averages (scaled by a multiplicative correction coefficient
  # to match first observation of each country)
  regionMapping <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
  numCountries <- nutrientPerArea
  numCountries[numCountries != 0] <- 1
  numCountries <- toolAggregate(numCountries, rel = regionMapping, weight = NULL,
                                from = "CountryCode", to = "RegionCode", dim = 1)
  worldRegionNutrientPerArea <- toolAggregate(nutrientPerArea, rel = regionMapping,
                                        weight = NULL, from = "CountryCode", to = "RegionCode", dim = 1) / numCountries

  .calibratingWorldAverages <- function(reg, x, data, glo = FALSE) {
    tmp <- x[reg, , ]
    if (isFALSE(glo)) {
      wr <- regionMapping$RegionCode[regionMapping$CountryCode == reg]
      data <- data[wr, , ]
    }
    yearsObs <- where(tmp != 0)$true$years
    yearsMissing <- where(tmp == 0)$true$years
    if (length(yearsObs) == 0) {
      tmp[, , ] <- data[, , drop = TRUE]
    } else if (length(yearsMissing) > 0) {
      y <- min(yearsObs)
      factor <- tmp[, y, ] / data[, y, , drop = TRUE] # multiplicative correction coefficient
      tmp[, yearsMissing, ] <- data[, yearsMissing, , drop = TRUE] * factor
    }
    return(tmp)
  }
  nutrientPerArea <- mbind(lapply(getItems(nutrientPerArea, dim = 1),
                                  .calibratingWorldAverages, nutrientPerArea, worldRegionNutrientPerArea))

  # fill gaps in fertilizer use by nutrient based on cropland and nutrient per area
  estFertByNutrient <- cropland * nutrientPerArea
  totalUseNutrients[totalUseNutrients == 0] <- estFertByNutrient[totalUseNutrients == 0]


  ## read FAO data on fertilizer by product and subset to fertilizer products of given nutrient type
  fertByProduct <- complete_magpie(readSource("FAO_online", "FertilizerProducts"), fill = 0)
  mapping <- toolGetMapping("fertilizer_products.csv", type = "sectoral", where = "mappingfolder")
  products <- mapping[mapping[, subtype] != "other", "product"]
  fertByProduct <- fertByProduct[, , products]
  totalUseProducts <- setNames(dimSums(fertByProduct[, , "Agricultural_Use_(tonnes)"], dim = 3.1),
                               paste0(subtype, "_fertilizer"))

  years <- intersect(getItems(totalUseNutrients, dim = 2), getItems(totalUseProducts, dim = 2))
  totalUseProducts <- totalUseProducts[, years, ]

  ## average nutrient content in fertilizer products
  avgNutrientContent <- totalUseNutrients[, years, , drop = TRUE] / totalUseProducts
  avgNutrientContent[avgNutrientContent > 1] <- 0
  avgNutrientContent[!is.finite(avgNutrientContent)] <- 0

  # fill gaps with world averages calibrated to countries
  numCountries <- avgNutrientContent
  numCountries[numCountries != 0] <- 1
  worldAvgNutrientContent <- dimSums(avgNutrientContent, dim = 1) / dimSums(numCountries, dim = 1)

  avgNutrientContent <- mbind(lapply(getItems(avgNutrientContent, dim = 1),
                                     .calibratingWorldAverages, avgNutrientContent, worldAvgNutrientContent, TRUE))

  ## fill gaps in total fertilizer use based on fertilizer by nutrient use
  estTotalUseProducts <- totalUseNutrients[, years, , drop = TRUE] / avgNutrientContent
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
