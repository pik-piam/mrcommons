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
#' @importFrom utils read.csv
#' @importFrom magclass setNames getCells collapseDim getItems
#' @importFrom magpiesets findset addLocation
#' @importFrom madrat toolAggregate toolGetMapping
#' @importFrom withr local_options

calcCroparea <- function(sectoral = "kcr", physical = TRUE, cellular = FALSE,
                         cells = "lpjcell", irrigation = FALSE) {

  local_options(magclass_sizeLimit = 1e+10)

  if (!cellular) {

    if (irrigation) stop("Irrigation levels for country based data not yet implemented!")

    #################################
    ### Croparea on country level ###
    #################################

    if (!is.null(sectoral) && !(sectoral == "lpj")) {

      cropPrim <- readSource("FAO_online", "Crop")[, , "area_harvested"]
      # use linear_interpolate
      fodder   <- readSource("FAO", "Fodder")[, , "area_harvested"]
      fodder   <- toolExtrapolateFodder(fodder, endyear = max(getYears(cropPrim, as.integer = TRUE)))
      data     <- toolFAOcombine(cropPrim, fodder) / 10^6 # convert to Mha

      if (sectoral %in% c("FoodBalanceItem", "kcr")) {

        aggregation <- toolGetMapping("FAOitems_online.csv", type = "sectoral",
                                      where = "mappingfolder")
        remove      <- setdiff(getNames(data, dim = 1), aggregation$ProductionItem)
        data        <- data[, , remove, invert = TRUE]
        data        <- toolAggregate(data, rel = aggregation, from = "ProductionItem",
                                     to = ifelse(sectoral == "kcr", "k", sectoral),
                                     dim = 3.1, partrel = TRUE)

        if (sectoral == "kcr") {

          # add bioenergy with 0 values
          data <- add_columns(x = data, addnm = c("betr", "begr"), dim = 3.1)
          data[, , c("betr", "begr")] <- 0

          # remove all non kcr items
          kcr    <- findset("kcr")
          remove <- setdiff(getItems(data, dim = 3.1), kcr)

          if (length(remove) > 0) {
            remainArea <- mean(dimSums(data[, , "remaining.area_harvested"], dim = 1) /
                                  dimSums(dimSums(data[, , "area_harvested"], dim = 3), dim = 1))
            if (remainArea > 0.02) vcat(1, "Aggregation created a 'remaining' category. The area harvested is",
                                         round(remainArea, digits = 3) * 100, "% of total \n")
            vcat(2, paste0("Data for the following items removed: ", remove))
            data <- data[, , kcr]
          }
        }

      } else if (sectoral != "ProductionItem") {
        stop("Sectoral aggregation not supported")
      }

    } else if (sectoral == "lpj") {

      magCroparea <- calcOutput("Croparea", sectoral = "kcr", physical = physical,
                                cellular = FALSE, irrigation = FALSE, aggregate = FALSE)
      mag2lpj     <- toolGetMapping(type = "sectoral", name = "MAgPIE_LPJmL.csv")
      mag2lpj     <- mag2lpj[!(mag2lpj$MAgPIE == "pasture"), ]
      lpjCroparea <- toolAggregate(magCroparea, rel = mag2lpj, from = "MAgPIE", to = "LPJmL", dim = 3.1)
      data        <- lpjCroparea

    } else {
      stop("Sectoral aggregation not supported")
    }

    # use the share of the single crops to calculate their "physical" area
    if (physical) {
      # 6620  = (6620|Arable land and Permanent crops or  6620|Cropland)
      cropland        <- setNames(collapseNames(calcOutput("FAOLand",
                                                           aggregate = FALSE)[, , "6620", pmatch = TRUE]), "crop")
      harvestedShare <- data / dimSums(data, dim = 3.1)
      commonyears     <- intersect(getYears(cropland), getYears(harvestedShare))
      data            <- collapseNames(cropland[, commonyears, ] * harvestedShare[, commonyears, ])
    }

    data[is.na(data)] <- 0

  } else {

    ##################################
    ### Croparea on cellular level ###
    ##################################

    if (sectoral == "kcr") {

      # LUH related data input on cell level
      luhWeights   <- calcOutput("LUH2MAgPIE", share = "MAGofLUH",
                                 missing = "fill", rice = "non_flooded", aggregate = FALSE)

      luhCroptypes <- c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")

      luhCroparea  <- calcOutput("LUH2v2", landuse_types = "LUH2v2",
                                 cells = cells, aggregate = FALSE, irrigation = irrigation,
                                 cellular = TRUE, selectyears = "past")
      if (cells == "magpiecell") {
        luhCroparea <- toolCell2isoCell(luhCroparea, cells = cells)
      }

      # Differentiation step that is necessary until full transition to 67k cells
      if (cells == "magpiecell") {
        commonCountries <- intersect(getItems(luhWeights, dim = "ISO"), getItems(luhCroparea, dim = "country"))
      } else if (cells == "lpjcell") {
        commonCountries <- intersect(getItems(luhWeights, dim = "ISO"), getItems(luhCroparea, dim = "iso"))
      } else {
        stop("Please select cellular data (mapgiecell or lpjcell) to be returned
              by calcCroparea when selecting cellular = TRUE")
      }

      # corrected rice area (in Mha)
      ricearea <- calcOutput("Ricearea", cellular = TRUE, cells = cells,
                             share = FALSE, aggregate = FALSE)

      # irrigation
      if (irrigation == TRUE) {

        # for check
        luhCropareaTotal <- dimSums(luhCroparea[, , luhCroptypes][, , "total"], dim = 3)

        # calculate irrigation share for rice area correction
        irrigShr <- new.magpie(cells_and_regions = getCells(luhCroparea),
                               years = getYears(luhCroparea),
                               names = getNames(luhCroparea), fill = NA)
        irrigShr <- irrigShr[, , "total", invert = TRUE]

        irrigShr[, , "irrigated"] <- collapseNames(ifelse(luhCroparea[, , "total"] > 0,
                                                          luhCroparea[, , "irrigated"] / luhCroparea[, , "total"], 0))
        irrigShr[, , "rainfed"]   <- 1 - collapseNames(irrigShr[, , "irrigated"])

        # flooded rice areas
        floodedRice <- collapseNames(ricearea[, , "flooded"] * irrigShr[, , "c3ann"])

        # reduce object size (if "total" is also reported magpie object grows too big (>1.3GB))
        luhCroparea <- luhCroparea[, , "total", invert = TRUE]

      } else {

        # for check
        luhCropareaTotal <- dimSums(luhCroparea[, , luhCroptypes], dim = 3)

        # flooded rice areas
        floodedRice <- collapseNames(ricearea[, , "flooded"])

      }

      # temporarily exclude flooded rice for distribution of other crops and aerobic rice areas
      luhCroparea[, , "c3ann"] <- luhCroparea[, , "c3ann"] - floodedRice

      # correction of LUH cropareas with FAO country shares
      luhCroparea      <- luhCroparea[, , luhCroptypes]
      luh2mag          <- luhCroparea * luhWeights[commonCountries, , ]
      magCroparea      <- dimSums(luh2mag, dim = 3.1)

      # total rice area correction
      magCroparea[, , "rice_pro"] <- magCroparea[, , "rice_pro"] + floodedRice

      # check sums
      if (any(round(abs(dimSums(magCroparea, dim = 3) - luhCropareaTotal), digits = 6) > 1e-6)) {
        stop("Sums after rice correction in calcCroparea don't match!")
      }

      data <- collapseNames(magCroparea)

    } else if (sectoral == "lpj") {

      magCroparea <- calcOutput("Croparea", sectoral = "kcr", physical = physical,
                                cellular = TRUE, irrigation = irrigation,
                                cells = cells, aggregate = FALSE)
      mag2lpj     <- toolGetMapping(type = "sectoral", name = "MAgPIE_LPJmL.csv")
      mag2lpj     <- mag2lpj[!(mag2lpj$MAgPIE == "pasture"), ]
      lpjCroparea <- toolAggregate(magCroparea, rel = mag2lpj, from = "MAgPIE", to = "LPJmL", dim = "MAG")
      data        <- lpjCroparea

    } else {
      stop("Not possible (for now) for the given item set (sectoral)!")
    }

    if (!physical) {

      multiCropping   <- calcOutput("Multicropping", aggregate = FALSE)

      if (cells == "magpiecell") {
        commonCountries <- intersect(getItems(multiCropping, dim = "ISO"), getItems(data, dim = "country"))
      } else if (cells == "lpjcell") {
        commonCountries <- intersect(getItems(multiCropping, dim = "ISO"), getItems(data, dim = "iso"))
      }

      data            <- data * multiCropping[commonCountries, getYears(data), ]
    }
  }

  data <- collapseNames(data)

  # not more precision than 1 ha needed. very small areas can make problems in some weighting scripts
  data <- round(data, 6)

  return(list(x            = data,
              weight       = NULL,
              unit         = "million ha",
              description  = "harvested crop areas from FAOSTAT",
              isocountries = !cellular))
}
