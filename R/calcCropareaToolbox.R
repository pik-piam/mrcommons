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
#' @param selectyears extract certain years from the data, y1950 - 2017 is available
#'
#' @return Magpie object with cropareas
#'
#' @author David Hoetten, Felicitas Beier
#'
#' @importFrom madrat readSource toolConditionalReplace toolCountryFill toolAggregate
#' @importFrom magclass dimSums getItems
#'
calcCropareaToolbox <- function(sectoral = "kcr", physical = TRUE, cellular = FALSE,
                                cells = "magpiecell", irrigation = FALSE,
                                selectyears = c("y1965", "y1970", "y1975",
                                                "y1980", "y1985", "y1990",
                                                "y1995", "y2000", "y2005",
                                                "y2010")) {
  if (is.numeric(selectyears)) {
    selectyears <- paste0("y", selectyears)
  }

  if (!cellular) {
    cropArea <- calcOutput("CropareaToolbox", sectoral = sectoral, physical = physical,
                           irrigation = irrigation, selectyears = selectyears,
                           cellular = TRUE, cells = "lpjcell", aggregate = FALSE)
    cropArea <- dimSums(cropArea, dim = c("x", "y"))
    cropArea <- toolConditionalReplace(x = toolCountryFill(cropArea),
                                       conditions = "is.na()", replaceby = 0)
  }

  harvestedArea <- readSource("LanduseToolbox", subtype = "harvestedArea")
  nonCrops      <- c("pasture")
  harvestedArea <- harvestedArea[, , nonCrops, invert = TRUE]

  if (!physical) {
    cropArea <- harvestedArea

  } else {
    physicalArea    <- readSource("LanduseToolbox", subtype = "physicalArea")
    physicalAreaSum <- dimSums(physicalArea, dim = "irrigation")

    # for the following crops we know that no multicropping is happening, so physical area = harvested area
    perennials <- c("sugr_cane", "oilpalm")
    crops      <- getItems(harvestedArea, dim = "crop")
    annuals    <- crops[!crops %in% perennials]

    # calculate the harvestedAreas for  different cropgroups
    perennialHarvestedA <- dimSums(harvestedArea[, , perennials], dim = c("crop", "irrigation"))
    annualsHarvestedA   <- dimSums(harvestedArea[, , annuals], dim = c("crop", "irrigation"))
    totalHarvestedA     <- perennialHarvestedA + annualsHarvestedA

    # check how  much physical area is remaining for the annuals after substracting the perennial physical area
    annualsPhysicalA <- physicalAreaSum - perennialHarvestedA # we can do that since for perennial physical=harvested

    # calculate a factor by which the annuals should be scaled down so the sum does not exceed annualsPhysicalA
    factorAnnuals <- ifelse(annualsPhysicalA > 0 & annualsHarvestedA > 0,
                             annualsPhysicalA / annualsHarvestedA,
                             1)

    # calculate a factor by which the all crops in mismatch cells (i.e. no annualPhyiscalA left) should be scaled down
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
  }

  if (cells == "magpiecell") {
    cropArea <- toolCoord2Isocell(cropArea)
  } else if (cells == "lpjcell") {
    # this is already the format of cropArea
  } else {
    stop("This value for the cell parameter not supported, choose between \"magpiecell\" and \"lpjcell\"")
  }

  if (sectoral == "kcr") {
    # this is already the format of cropArea
  } else if (sectoral == "lpj") {
    mapMagToLpj <- toolGetMapping(type = "sectoral", name = "MAgPIE_LPJmL.csv")
    mapMagToLpj <- mapMagToLpj[!(mapMagToLpj$MAgPIE %in% nonCrops), ]
    cropArea    <- toolAggregate(cropArea, rel = mapMagToLpj,
                                 from = "MAgPIE", to = "LPJmL", dim = "crop")
  } else {
    stop("This sectoral aggregation is not availiable in calcCropareaToolbox")
  }

  if (irrigation == TRUE) {
    # this is already the format of cropArea
  } else {
    years        <- getItems(cropArea, dim = "year")
    cropAreaList <- vector(mode = "list", length = length(years))
    for (y in seq(1, length(years))) {
      cropAreaList[[y]] <- dimSums(cropArea[, years[y], ], dim = "irrigation")
    }
    cropArea <- mbind(cropAreaList)
    rm("cropAreaList")
  }

  # check consitency with calcFallowLand
  if (physical == TRUE) {
    fallow <- calcOutput("FallowLand", aggregate = FALSE)

    if (irrigation == TRUE) {
      physicalCropSum <- dimSums(cropArea, dim = c("crop", "irrigation"))
    } else {
      physicalCropSum <- dimSums(cropArea, dim = c("crop"))
    }

    if (any(abs(physicalCropSum + fallow - physicalAreaSum) > 10^-16)) {
      stop("Sum of crops + fallow land doesn't match with total physical cropland.")
    }
  }

  if (all(selectyears %in% getItems(cropArea, dim = "year"))) {
    cropArea <- cropArea[, selectyears, ]
  } else {
    stop("The selected years are not supported (0nly 1950 - 2017).")
  }

  return(list(x = cropArea,
              weight = NULL,
              description = "Croparea for different croptypes",
              unit = "mha",
              isocountries = FALSE))
}
