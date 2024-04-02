#' @title calcCropareaLandInG
#' @description This function uses total physical area and
#'              crop-specific harvested area data from LandInG
#'              to calculate crop-specific physical and harvested
#'              areas considering special rules
#'              for the allocation of perennial and annual crops.
#'
#' @param sectoral    "kcr" MAgPIE items, and "lpj" LPJmL items
#' @param physical    if TRUE the sum over all crops plus fallow land (of calcFallowLand)
#'                    agrees with the physical cropland of readLandInG(subtype = physical)
#' @param cellular    if TRUE: calculates cellular crop area for all magpie croptypes.
#'                    Option FALSE is not (yet) available.
#' @param cells       Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param irrigation  If true: cellular areas are returned separated
#'                    into irrigated and rainfed
#' @param selectyears extract certain years from the data
#' @param lpjml       LPJmL version used to determine multiple cropping suitability
#' @param climatetype Climate scenario or historical baseline "GSWP3-W5E5:historical"
#'                    used to determine multiple cropping suitability
#'
#' @return MAgPIE object with cropareas
#'
#' @author David Hoetten, Felicitas Beier
#'
#' @importFrom madrat readSource toolConditionalReplace toolCountryFill toolAggregate
#' @importFrom magclass dimSums getItems
#' @importFrom mstools toolHoldConstant
#'
calcCropareaLandInG <- function(sectoral = "kcr", physical = TRUE, cellular = FALSE,
                                cells = "magpiecell", irrigation = FALSE, selectyears = "all",
                                lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                                          crop = "ggcmi_phase3_nchecks_bft_e511ac58"),
                                climatetype = "GSWP3-W5E5:historical") {

  if (climatetype != "GSWP3-W5E5:historical" ||
        lpjml[["crop"]] != "ggcmi_phase3_nchecks_bft_e511ac58") {
    warning("Error in calcCropareaLandInG: The LPJmL version has been updated
            since LandInG was run for the last time.
            Please consider updating the LandInG data.")
    # Kristine: How to include a mapping here?
  }

  withr::local_options(magclass_sizeLimit = 1e+12)

  ### Read in data ###
  # total physical area from LandInG (in Mha)
  physicalArea <- readSource("LandInG", subtype = "physicalArea")
  # crop-specific harvested area (in Mha)
  harvestedArea <- readSource("LandInG", subtype = "harvestedArea")

  ### Calculations ###
  # read in fallow land (for check below)
  fallow <- calcOutput("FallowLand", aggregate = FALSE)

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
    fallow <- toolHoldConstant(fallow, selectyears)
  }

  # reduce harvested area to crop area
  nonCrops      <- c("pasture")
  harvestedArea <- harvestedArea[, , nonCrops, invert = TRUE]

  # croplists
  crops      <- getItems(harvestedArea, dim = "crop")
  perennials <- c("sugr_cane", "oilpalm")
  annuals    <- crops[!crops %in% perennials]

  # Reduce to selected number of years
  # and split calculation into single years for memory reasons
  cropAreaList        <- vector(mode = "list", length = length(selectyears))
  names(cropAreaList) <- selectyears
  for (y in selectyears) {
    # select year
    physicalAreaYearly  <- physicalArea[, y, ]
    harvestedAreaYearly <- harvestedArea[, y, ]

    ##################################
    ## Crop-specific physical areas ##
    ##################################
    # Total physical area (in Mha)
    physicalAreaSum <- dimSums(physicalAreaYearly, dim = "irrigation")

    # Calculate the total harvested areas for different crop groups
    # for perennial crops no multicropping is happening, so physical area = harvested area
    perennialHarvestedA <- dimSums(harvestedAreaYearly[, , perennials], dim = c("crop", "irrigation"))
    annualsHarvestedA   <- dimSums(harvestedAreaYearly[, , annuals], dim = c("crop", "irrigation"))
    totalHarvestedA     <- perennialHarvestedA + annualsHarvestedA

    # Check how much physical area is remaining for the annuals after subtracting the perennial physical area
    annualsPhysicalA <- physicalAreaSum - perennialHarvestedA

    # Calculate a factor by which the annuals should be scaled down so the sum does not exceed annualsPhysicalA
    factorAnnuals <- ifelse(annualsPhysicalA > 0 & annualsHarvestedA > 0,
                            annualsPhysicalA / annualsHarvestedA,
                            1)

    # Calculate a factor by which all crops in mismatch cells (i.e. no annualPhyiscalA left) should be scaled down
    factorMismatches <- ifelse(annualsPhysicalA <= 0 & totalHarvestedA > 0,
                               physicalAreaSum / totalHarvestedA,
                               1)

    # Only scale crops down not up (i.e. keep fallow land)
    factorAnnuals[factorAnnuals > 1]       <- 1
    factorMismatches[factorMismatches > 1] <- 1

    # Apply the factors
    physicalAreaYearly <- harvestedAreaYearly
    physicalAreaYearly[, , annuals] <- harvestedAreaYearly[, , annuals] * factorAnnuals
    physicalAreaYearly <- physicalAreaYearly * factorMismatches

    # Clean up for memory reasons
    rm(factorMismatches, factorAnnuals)

    ###################################
    ## Correction of harvested areas ##
    ###################################
    # Correction of perennial harvested area required
    # due to above allocation of crops distinguishing
    # annuals and perennials

    # Check whether more than 5% of harvested area would be lost
    if (any(dimSums(harvestedAreaYearly[, , perennials] - physicalAreaYearly[, , perennials],
                    dim = c(1, 3.2)) / dimSums(harvestedAreaYearly, dim = c(1, 3.2)) * 100 > 5)) {
      stop("More than 5% of global harvested area is lost through perennial area correction")
    }
    # Check whether more than 10% of harvested area would be lost in any country
    # that has more than 100 000 ha total harvested area
    if (any(dimSums(harvestedAreaYearly,
                    dim = c(1.1, 1.2, 3)) > 0.1 &
              (dimSums(harvestedAreaYearly[, , perennials] - physicalAreaYearly[, , perennials],
                       dim = c(1.1, 1.2, 3)) / dimSums(harvestedAreaYearly,
                                                       dim = c(1.1, 1.2, 3)) * 100) > 10,
            na.rm = TRUE)) {
      stop(paste0("Some countries (with more than 100 000 ha harvested area) would loose more than 10% in year ", y))
    }

    # In the allocation of perennials to physical area, some harvested area is lost and needs to be corrected
    harvestedAreaYearly[, , perennials] <- physicalAreaYearly[, , perennials]

    ###########################################
    ## Correction of multiple cropping cases ##
    ###########################################
    # In the LandInG calculations, some rainfed harvested area is allocated to irrigated land.
    # This leads to cases where areas are declared as "rainfed harvested area" resulting in
    # cropping intensities > 1 for rainfed crops where not multiple cropping is possible
    # according to the multiple cropping suitability.
    # These areas are declared irrigated in the following correction.

    ### Read in data ###
    # Crop-specific and irrigation-type specific multiple cropping suitability
    mcSuit <- calcOutput("MulticroppingSuitability", selectyears = y,
                         lpjml = lpjml, climatetype = climatetype,
                         suitability = "endogenous", sectoral = "kcr",
                         aggregate = FALSE)
    mcSuit <- dimOrder(mcSuit, c(2, 1), dim = 3)
    mcSuit <- mcSuit[, , getItems(harvestedAreaYearly, dim = 3)]

    # Sanity checks
    if (any(harvestedAreaYearly != 0 & physicalAreaYearly == 0)) {
      stop("Please check calcCropareaLandInG. The following calculations area based on the
            assumption that there is no harvested area where no physical area exists.")
    }
    # Crop- and irrigation-specific cropping intensity
    fctMCwhereNonSuit <- function(physicalAreaYearly = physicalAreaYearly,
                                  harvestedAreaYearly = harvestedAreaYearly,
                                  mcSuit = mcSuit) {
      ci <- ifelse(physicalAreaYearly > 0, harvestedAreaYearly / physicalAreaYearly, 1)
      # Boolean: is there multiple cropping or not?
      mcCurr <- ci
      mcCurr[, , ] <- 0
      mcCurr[ci > (1 + 1e-3)] <- 1
      # Multiple cropping where non-suitable for multiple cropping
      violation <- mcCurr == 1 & mcSuit == 0

      return(violation)
    }

    # Multiple cropping where non-suitable for multiple cropping
    violation <- fctMCwhereNonSuit(physicalAreaYearly = physicalAreaYearly,
                                   harvestedAreaYearly = harvestedAreaYearly,
                                   mcSuit = mcSuit)
    rfViolation <- collapseNames(violation[, , "rainfed"])

    # Temporary objects with correct dimensionality
    harvIR <- collapseNames(harvestedAreaYearly[, , "irrigated"])
    physIR <- collapseNames(physicalAreaYearly[, , "irrigated"])
    harvRF <- collapseNames(harvestedAreaYearly[, , "rainfed"])
    physRF <- collapseNames(physicalAreaYearly[, , "rainfed"])

    # Add multiple cropped rainfed areas to harvested irrigated areas
    harvIR[rfViolation] <- harvIR[rfViolation] + harvRF[rfViolation] - physRF[rfViolation]
    # Reduce harvested rainfed areas where not suitable to physical rainfed:
    harvRF[rfViolation] <- physRF[rfViolation]

    # Areas where no physical irrigated area was available, but now received harvested area
    # have to be re-declared to irrigated physical areas.
    # (Note: this can occur due to physical area correction)
    noPhysical <- (harvIR != 0 & physIR == 0)

    # Allocate areas that are declared rainfed to these irrigated areas
    physIR[noPhysical] <- physIR[noPhysical] + physRF[noPhysical]
    harvIR[noPhysical] <- harvIR[noPhysical] + physRF[noPhysical]
    physRF[noPhysical] <- 0
    harvRF[noPhysical] <- physRF[noPhysical]

    # Overwrite original object with corrected values
    harvestedAreaYearly[, , "irrigated"] <- harvIR
    harvestedAreaYearly[, , "rainfed"]   <- harvRF
    physicalAreaYearly[, , "irrigated"]  <- physIR
    physicalAreaYearly[, , "rainfed"]    <- physRF

    rm(harvIR, harvRF, physIR, physRF)

    # Check whether multiple cropping has been corrected
    violation <- fctMCwhereNonSuit(physicalAreaYearly = physicalAreaYearly,
                                   harvestedAreaYearly = harvestedAreaYearly,
                                   mcSuit = mcSuit)
    if (any(violation)) {
      stop("Not all cases where multiple cropping happens
      despite not being suitable for multiple cropping have been corrected.")
    }

    ###################
    ## Select output ##
    ###################
    if (!physical) {
      cropArea <- harvestedAreaYearly
    } else {
      cropArea <- physicalAreaYearly
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
      stop("This sectoral aggregation is not available in calcCropareaLandInG")
    }

    if (irrigation == TRUE) {
      # this is already the format of cropArea
    } else {
      cropArea <- dimSums(cropArea, dim = "irrigation")
    }

    # Check consistency with calcFallowLand
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
