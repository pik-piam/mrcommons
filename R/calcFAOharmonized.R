#' @title calcFAOharmonized
#' @description Calculate harmonized FAO Commodity Balance and Food Supply data based on CB, only harvested areas
#'              are taken from ProdSTAT. This functions adds the CBCrop, CBLive, FSCrop and FSLive data together.
#'
#' @return FAO harmonized data, weight as NULL, and a description as as a list of MAgPIE objects
#' @author Ulrich Kreidenweis, David Chen, Kristine Karstens
#' @examples
#' \dontrun{
#' a <- calcOutput("FAOharmonized")
#' }
#' @importFrom utils read.csv

calcFAOharmonized <- function() {


  # input data: Commodity Balance (Crops Primary + Livestock Primary), Food Supply (Crops Primary + Livestock Primary)
  cbCrop <- readSource("FAO_online", "CBCrop")
  cbLive <- readSource("FAO_online", "CBLive")
  fsCrop <- readSource("FAO_online", "FSCrop")
  fsLive <- readSource("FAO_online", "FSLive")

  cb <- toolFAOcombine(cbLive, cbCrop, combine = "Item")
  fs <- toolFAOcombine(fsLive, fsCrop, combine = "Item")
  rm(cbCrop, cbLive, fsCrop, fsLive)

  faoData <- mbind(cb, fs)

   ## in addition harvested area from Crops Primary

  prod <- readSource("FAO_online", "Crop", convert = TRUE)

  ## aggregate Prod to CB units
  aggregation <- toolGetMapping("FAOitems_online.csv", type = "sectoral", where = "mappingfolder")

    # remove  aggregate categories
  remove <- setdiff(getNames(prod, dim = 1), aggregation$ProductionItem)
  prod <- prod[, , remove, invert = TRUE]

  areaHarvested <- toolAggregate(prod, rel = aggregation, from = "ProductionItem", to = "FoodBalanceItem",
                                  dim = 3.1, partrel = TRUE)[, , "area_harvested"]

  commonyears <- intersect(getYears(areaHarvested), getYears(faoData))

  faoData <- mbind(faoData[, commonyears, ], areaHarvested[, commonyears, ])

  rm(areaHarvested)


  ### add Fodder data ###

  fodder <- readSource("FAO", "Fodder")
  fodder <- toolExtrapolateFodder(fodder, endyear = max(getYears(faoData, as.integer = TRUE)))
  fodder <- add_columns(x = fodder, addnm = "domestic_supply", dim = 3.2)
  fodder[, , "domestic_supply"] <- fodder[, , "feed"]
  fodderAggregated <- toolAggregate(fodder, rel = aggregation, from = "ProductionItem",
                                    to = "FoodBalanceItem", dim = 3.1, partrel = TRUE)
  cyears <- intersect(getYears(faoData), getYears(fodderAggregated))
  faoData <- mbind(faoData[, cyears, ], fodderAggregated[, cyears, ])
  rm(fodder, fodderAggregated)
gc()

  faoData[is.na(faoData)] <- 0

  ## check if there is data without an element name

  ## what to do? In case there is data these dimensions should not be deleted

  if (any(getItems(faoData, dim = 3.1) == "")) {
    if (sum(faoData[, , ""]) == 0) {
      faoData <- faoData[, , "", invert = TRUE]
    } else  {
      vcat(1, 'Aggregation created entries without name (""), but containing information. This should not be the case.')
    }
  }

  if (any(getNames(faoData) == "remaining.production")) {
    remainProd <- mean(dimSums(faoData[, , "remaining.production"], dim = 1) /
                         dimSums(dimSums(faoData[, , "production"], dim = 3), dim = 1))
    if (remainProd > 0.02) vcat(1, "Aggregation created a 'remaining' category. Production is",
                                round(remainProd, digits = 3) * 100, "% of total \n")
  }
  if (any(getNames(faoData) == "remaining.area_harvested")) {
    remainArea <- mean(dimSums(faoData[, , "remaining.area_harvested"], dim = 1) /
                         dimSums(dimSums(faoData[, , "area_harvested"], dim = 3), dim = 1))
    if (remainArea > 0.02) vcat(1, "Aggregation created a 'remaining' category. The area harvested is",
                                round(remainArea, digits = 3) * 100, "% of total \n")
  }

  # conversion from tonnes to Mt, hectares to Mha and 10^6kcal to 10^12kcal.
  faoData <- faoData / 10^6

  return(list(x = faoData,
              weight = NULL,
              description = "FAO Commodity Balance and Food Supply data",
              unit = "Unit in Mt/yr, for area Mha, calories in 10^12 kcal/yr",
              note = "food_supply_kcal, protein_supply and fat_supply were calculated from per capita per day values"))
}
