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

  withr::local_options(magclass_sizeLimit = 1e+12)

  ### Read in data ###
  # total physical area from LandInG (in Mha)
  physicalArea <- readSource("LanduseToolbox", subtype = "physicalArea")
  # crop-specific harvested area (in Mha)
  harvestedArea <- readSource("LanduseToolbox", subtype = "harvestedArea")

  if (physical) {
    # read in fallow land (for check below)
    fallow <- calcOutput("FallowLand", aggregate = FALSE)
  } else {
    # crop-specific physical area (in Mha)
    physicalAreaCrop <- calcOutput("CropareaToolbox",
                                   sectoral = "kcr", physical = TRUE, cellular = TRUE,
                                   cells = "lpjcell", irrigation = TRUE, selectyears = selectyears,
                                   aggregate = FALSE)
  }

  # year selection
  if (any(selectyears == "all")) {
    selectyears <- getItems(physicalArea, dim = "year")
  }
  if (is.numeric(selectyears)) {
    selectyears <- paste0("y", selectyears)
  }

  # extrapolate years
  if (!all(selectyears %in% getItems(physicalArea, dim = "year"))) {
    physicalArea  <- toolHoldConstant(physicalArea, selectyears)
    harvestedArea <- toolHoldConstant(harvestedArea, selectyears)
    if (physical) {
      fallow <- toolHoldConstant(fallow, selectyears)
    }
  }

  # reduce to selected number of years
  # and split calculation into single years for memory reasons
  cropAreaList        <- vector(mode = "list", length = length(selectyears))
  names(cropAreaList) <- selectyears
  for (y in selectyears) {

    # select year
    physicalAreaYearly  <- physicalArea[, y, ]
    harvestedAreaYearly <- harvestedArea[, y, ]

    # reduce harvested area to crop area
    nonCrops            <- c("pasture")
    harvestedAreaYearly <- harvestedAreaYearly[, , nonCrops, invert = TRUE]

    # croplists
    crops      <- getItems(harvestedAreaYearly, dim = "crop")
    perennials <- c("sugr_cane", "oilpalm")
    annuals    <- crops[!crops %in% perennials]

    ### Crop-specific harvested area calculation: ###
    if (!physical) {

      # Harvested area by crop as of LandInG data
      cropArea <- harvestedAreaYearly

      # Physical area by crop as calculated in this function
      physicalAreaCropYearly <- physicalAreaCrop[, y, ]

      ### Correction of perennial harvested area ###
      # Check whether more than 5% of harvested area would be lost
      if (any(dimSums(cropArea[, , perennials] - physicalAreaCropYearly[, , perennials],
                      dim = c(1, 3.2)) / dimSums(cropArea, dim = c(1, 3.2)) * 100 > 5)) {
        stop("More than 5% of global harvested area is lost through perennial area correction")
      }
      # Check whether more than 10% of harvested area would be lost in any country
      # that has more than 100 000 ha total harvested area
      if (any(dimSums(cropArea, dim = c(1.1, 1.2, 3)) > 0.1 &
                (dimSums(cropArea[, , perennials] - physicalAreaCropYearly[, , perennials],
                         dim = c(1.1, 1.2, 3)) / dimSums(cropArea, dim = c(1.1, 1.2, 3)) * 100) > 10,
              na.rm = TRUE)) {
        stop(paste0("Some countries (with more than 100 000 ha harvested area) would loose more than 10% in year ", y))
      }

      # In the allocation of perennials to physical area, some harvested area is lost and needs to be corrected
      cropArea[, , perennials] <- physicalAreaCropYearly[, , perennials]

    } else {
      ### Crop-specific physical area calculation: ###
      physicalAreaSum <- dimSums(physicalAreaYearly, dim = "irrigation")

      # calculate the total harvested areas for different cropgroups
      # for perennial crops no multicropping is happening, so physical area = harvested area
      perennialHarvestedA <- dimSums(harvestedAreaYearly[, , perennials], dim = c("crop", "irrigation"))
      annualsHarvestedA   <- dimSums(harvestedAreaYearly[, , annuals], dim = c("crop", "irrigation"))
      totalHarvestedA     <- perennialHarvestedA + annualsHarvestedA

      # check how much physical area is remaining for the annuals after subtracting the perennial physical area
      annualsPhysicalA <- physicalAreaSum - perennialHarvestedA

      # calculate a factor by which the annuals should be scaled down so the sum does not exceed annualsPhysicalA
      factorAnnuals <- ifelse(annualsPhysicalA > 0 & annualsHarvestedA > 0,
                              annualsPhysicalA / annualsHarvestedA,
                              1)

      # calculate a factor by which all crops in mismatch cells (i.e. no annualPhyiscalA left) should be scaled down
      factorMismatches <- ifelse(annualsPhysicalA <= 0 & totalHarvestedA > 0,
                                 physicalAreaSum[, , ] / totalHarvestedA,
                                 1)

      # only scale crops down not up (i.e. keep fallow land)
      factorAnnuals[factorAnnuals > 1]       <- 1
      factorMismatches[factorMismatches > 1] <- 1

      # apply the factors
      physicalAreaCrop <- harvestedAreaYearly
      physicalAreaCrop[, , annuals] <- harvestedAreaYearly[, , annuals] * factorAnnuals
      physicalAreaCrop <- physicalAreaCrop * factorMismatches

      cropArea <- physicalAreaCrop
      rm(factorMismatches, factorAnnuals, physicalAreaCrop)
    }

    if (sectoral == "kcr") {
      # this is already the format of cropArea
    } else if (sectoral == "lpj") {
      # crop mapping
      mapMagToLpj <- toolGetMapping(type = "sectoral", name = "MAgPIE_LPJmL.csv",
                                    where = "mappingfolder")
      mapMagToLpj <- mapMagToLpj[!(mapMagToLpj$MAgPIE %in% nonCrops), ]

      cropArea <- toolAggregate(cropArea, rel = mapMagToLpj,
                                from = "MAgPIE", to = "LPJmL", dim = "crop")
    } else {
      stop("This sectoral aggregation is not available in calcCropareaToolbox")
    }

    if (irrigation == TRUE) {
      # this is already the format of cropArea
    } else {
      cropArea <- dimSums(cropArea, dim = "irrigation")
    }

    # check consistency with calcFallowLand
    if (physical == TRUE) {
      if (irrigation == TRUE) {
        physicalCropSum <- dimSums(cropArea, dim = c("crop", "irrigation"))
      } else {
        physicalCropSum <- dimSums(cropArea, dim = c("crop"))
      }

      if (any(abs(physicalCropSum + fallow[, y, ] - physicalAreaSum) > 10^-16)) {
        stop("Sum of crops + fallow land doesn't match with total physical cropland.")
      }
    }

    # Aggregation to iso-level
    if (!cellular) {
      # aggregate to countries
      cropArea <- dimSums(cropArea, dim = c("x", "y"))
      # fill missing countries with 0
      cropArea <- toolConditionalReplace(x = toolCountryFill(cropArea),
                                         conditions = "is.na()", replaceby = 0)
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
    cropAreaList[[y]] <- cropArea
  }
  # bind years together
  out <- mbind(cropAreaList)

  return(list(x = out,
              weight = NULL,
              description = "Croparea for different croptypes",
              unit = "Mha",
              isocountries = FALSE))
}
