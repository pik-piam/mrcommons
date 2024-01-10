#' @title calcCroparea
#' @description Returns harvested areas of individual crops from FAOSTAT.
#'              Total harvested areas can be lower or higher than arable
#'              land because of multicropping or fallow land.
#'              Rice areas are distributed to flooded LUH areas. Additional FAOSTAT
#'              rice areas are distributed based on country shares.
#'
#' @param sectoral   "area_harvested" returns croparea aggregated to FAO products,
#'                   "ProductionItem" unaggregated ProdSTAT items,
#'                   "FoodBalanceItem" Food Balance Sheet categories,
#'                   "kcr" MAgPIE items, and "lpj" LPJmL items
#' @param physical   if TRUE the sum over all crops agrees with the cropland area per country
#' @param cellular   if TRUE: calculates cellular MAgPIE crop area for all magpie croptypes.
#'                   Crop area from LUH2 crop types (c3ann, c4ann, c3per, c4per, cnfx)
#'                   are mapped to MAgpIE crop types using mappingLUH2cropsToMAgPIEcrops.csv.
#'                   Harvested areas of FAO weight area within a specific LUH crop type
#'                   to divide into MAgPIE crop types.
#' @param cells      Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param irrigation If true: cellular areas are returned separated
#'                   into irrigated and rainfed (see setup in calcLUH2v2)
#'
#' @return areas of individual crops from FAOSTAT and weight
#'
#' @author Ulrich Kreidenweis, Kristine Karstens, Felicitas Beier
#'
#' @importFrom magpiesets findset

calcCroparea <- function(sectoral = "kcr", physical = TRUE, cellular = FALSE,
                         cells = "lpjcell", irrigation = FALSE, selectyears = "past",
                         datasource = "LandInG") {

  if (selectyears == "past") {
    selectyears <- findset("past")
  }

  if (datasource == "LandInG") {
    data <- calcOutput("CropareaLandInG", sectoral = sectoral, physical = physical,
                       cellular = cellular, cells = cells, irrigation = irrigation,
                       selectyears = selectyears, aggregate = FALSE)
  } else if (datasource == "FAOLUH") {
    if (any(!(selectyears %in% findset("past")))) {
      stop("only yerars in tpast support for CropareaFAOLUH")
    }
    data <- calcOutput("CropareaFAOLUH", sectoral = sectoral, physical = physical,
                       cellular = cellular, cells = cells, irrigation = irrigation,
                       aggregate = FALSE)
    data <- data[, selectyears, ]
  } else {
    stop("datsource unknown")
  }
  return(list(x            = data,
              weight       = NULL,
              unit         = "million ha",
              description  = "croparea",
              isocountries = !cellular))
}
