#' @title calcCropareaToolbox
#' @description This function uses the data from the LPJmL io Toolbox
#'              to calculate cropareas in various formats.
#'
#' @param sectoral   "kcr" MAgPIE items, and "lpj" LPJmL items
#' @param physical   if TRUE the sum over all crops plus fallow land (of calcFallowLand)
#'                   agrees with the physical cropland of readLanduseToolbox(subtype = physical)
#' @param cellular   if TRUE: calculates cellular crop area for all magpie croptypes.
#'                   Option FALSE is not (yet) available.
#' @param cells      Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param irrigation If true: cellular areas are returned separated
#'                   into irrigated and rainfed
#' @param selectyears extract certain years from the data
#'
#' @return MAgPIE object with cropareas
#'
#' @author David Hoetten, Felicitas Beier
#'
#' @importFrom madrat readSource toolConditionalReplace toolCountryFill toolAggregate
#' @importFrom magclass dimSums getItems
#' @importFrom mstools toolHoldConstant
#'
calcCropareaToolbox <- function(sectoral = "kcr", physical = TRUE, cellular = FALSE,
                                cells = "magpiecell", irrigation = FALSE, selectyears = "all") {

  harvestedArea <- readSource("LanduseToolbox", subtype = "harvestedArea")
  nonCrops      <- c("pasture")
  harvestedArea <- harvestedArea[, , nonCrops, invert = TRUE]
  perennials    <- c("sugr_cane", "oilpalm")

  if (any(selectyears == "all")) {
    selectyears <- getItems(harvestedArea, dim = 2)
  }
  if (is.numeric(selectyears)) {
    selectyears <- paste0("y", selectyears)
  }

  if (!physical) {
    # harvested area by crop as of LandInG data
    cropArea <- harvestedArea

    # physical area by crop as calculated in this function
    physArea <- calcOutput("CropareaToolbox", sectoral = "kcr", physical = TRUE, cellular = TRUE,
                           cells = "lpjcell", irrigation = TRUE, selectyears = selectyears,
                           aggregate = FALSE)

    # Check whether more than 5% of harvested area would be lost
    if (any(dimSums(cropArea[, , perennials] - physArea[, , perennials],
                    dim = c(1, 3.2)) / dimSums(cropArea, dim = c(1, 3.2)) * 100) > 5) {
      stop("More than 5% of global harvested area is lost through perennial area correction")
    }
    # Check whether more than 10% of harvested area would be lost in any country
    # that has more than 100 000 ha total harvested area
    # (year loop due to memory issues)
    # (note: the loop takes 1.197 min. Check for performance improvement)
    for (y in selectyears) {
      if (any(dimSums(cropArea[, y, ], dim = c(1.1, 1.2, 3)) > 0.1 &
                (dimSums(cropArea[, y, perennials] - physArea[, y, perennials],
                         dim = c(1.1, 1.2, 3)) / dimSums(cropArea[, y, ], dim = c(1.1, 1.2, 3)) * 100) > 10,
              na.rm = TRUE)) {
        stop(paste0("Some countries (with more than 100 000 ha harvested area) would loose more than 10% in year ", y))
      }
    }

    # in the allocation of perennials to physical area, some harvested area is lost and needs to be corrected
    cropArea[, , perennials] <- physArea[, , perennials]

  } else {
    # total physical are from LandInG
    physicalArea    <- readSource("LanduseToolbox", subtype = "physicalArea")
    physicalAreaSum <- dimSums(physicalArea, dim = "irrigation")

    # for the perennial crops no multicropping is happening, so physical area = harvested area
    crops      <- getItems(harvestedArea, dim = "crop")
    annuals    <- crops[!crops %in% perennials]

    # calculate the total harvestedAreas for different cropgroups
    perennialHarvestedA <- dimSums(harvestedArea[, , perennials], dim = c("crop", "irrigation"))
    annualsHarvestedA   <- dimSums(harvestedArea[, , annuals], dim = c("crop", "irrigation"))
    totalHarvestedA     <- perennialHarvestedA + annualsHarvestedA

    # check how  much physical area is remaining for the annuals after subtracting the perennial physical area
    annualsPhysicalA <- physicalAreaSum - perennialHarvestedA # we can do that since for perennial physical=harvested

    # calculate a factor by which the annuals should be scaled down so the sum does not exceed annualsPhysicalA
    factorAnnuals <- ifelse(annualsPhysicalA > 0 & annualsHarvestedA > 0,
                            annualsPhysicalA / annualsHarvestedA,
                            1)

    # calculate a factor by which all crops in mismatch cells (i.e. no annualPhyiscalA left) should be scaled down
    factorMismatches <- ifelse(annualsPhysicalA <= 0 & totalHarvestedA > 0,
                               physicalAreaSum / totalHarvestedA,
                               1)

    # only scale crops down not up (i.e. keep fallow land)
    factorAnnuals[factorAnnuals > 1]       <- 1
    factorMismatches[factorMismatches > 1] <- 1

    # apply the factors
    physicalAreaCrop <- harvestedArea
    physicalAreaCrop[, , annuals] <- harvestedArea[, , annuals] * factorAnnuals
    physicalAreaCrop <- physicalAreaCrop * factorMismatches

    cropArea <- physicalAreaCrop
    rm(factorMismatches, factorAnnuals, physicalAreaCrop)
  }

  if (sectoral == "kcr") {
    # this is already the format of cropArea
  } else if (sectoral == "lpj") {
    mapMagToLpj <- toolGetMapping(type = "sectoral", name = "MAgPIE_LPJmL.csv",
                                  where = "mappingfolder")
    mapMagToLpj <- mapMagToLpj[!(mapMagToLpj$MAgPIE %in% nonCrops), ]
    cropArea    <- toolAggregate(cropArea, rel = mapMagToLpj,
                                 from = "MAgPIE", to = "LPJmL", dim = "crop")
  } else {
    stop("This sectoral aggregation is not availiable in calcCropareaToolbox")
  }

  if (irrigation == TRUE) {
    # this is already the format of cropArea
  } else {
    # split into single years for memory reasons
    years        <- getItems(cropArea, dim = "year")
    cropAreaList <- vector(mode = "list", length = length(years))
    for (y in seq(1, length(years))) {
      cropAreaList[[y]] <- dimSums(cropArea[, years[y], ], dim = "irrigation")
    }
    cropArea <- mbind(cropAreaList)
    rm("cropAreaList")
  }

  # check consistency with calcFallowLand
  if (physical == TRUE) {
    fallow <- calcOutput("FallowLand", aggregate = FALSE)

    if (irrigation == TRUE) {
      # split into single years for memory reasons
      years               <- getItems(cropArea, dim = "year")
      physicalCropSumList <- vector(mode = "list", length = length(years))
      for (y in seq(1, length(years))) {
        physicalCropSumList[[y]] <- dimSums(cropArea[, years[y], ], dim = c("crop", "irrigation"))

      }
      physicalCropSum <- mbind(physicalCropSumList)
      rm(physicalCropSumList)
    } else {
      physicalCropSum <- dimSums(cropArea, dim = c("crop"))
    }

    if (any(abs(physicalCropSum + fallow - physicalAreaSum) > 10^-16)) {
      stop("Sum of crops + fallow land doesn't match with total physical cropland.")
    }
  }

  # Aggregation to iso-level
  if (!cellular) {
    # split into single years for memory reasons
    years               <- getItems(cropArea, dim = "year")
    cropAreaList <- vector(mode = "list", length = length(years))
    for (y in seq(1, length(years))) {
      cropAreaList <- calcOutput("CropareaToolbox", sectoral = sectoral, physical = physical,
                                 irrigation = irrigation,
                                 cellular = TRUE, cells = "lpjcell", aggregate = FALSE)[, years, ]
      cropAreaList <- dimSums(cropAreaList, dim = c("x", "y"))
      cropAreaList <- toolConditionalReplace(x = toolCountryFill(cropAreaList),
                                             conditions = "is.na()", replaceby = 0)
    }
    cropArea <- mbind(cropAreaList)
    rm("cropAreaList")
  } else {

    if (cells == "magpiecell") {
      cropArea <- toolCoord2Isocell(cropArea)
    } else if (cells == "lpjcell") {
      # this is already the format of cropArea
    } else {
      stop("This value for the cell parameter is not supported,
            choose between \"magpiecell\" and \"lpjcell\"")
    }
  }

  # extrapolate years
  if (!all(selectyears %in% getItems(cropArea, dim = "year"))) {
    cropArea <- toolHoldConstant(cropArea, selectyears)
  }

  # reduce to selected number of years
  cropArea <- cropArea[, selectyears, ]

  return(list(x = cropArea,
              weight = NULL,
              description = "Croparea for different croptypes",
              unit = "Mha",
              isocountries = FALSE))
}
