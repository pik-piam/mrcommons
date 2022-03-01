#' @title calcYields
#' @description This function extracts yields from LPJmL to MAgPIE
#'
#' @param source Defines LPJmL version for main crop inputs and isimip replacement.
#' For isimip choose crop model/gcm/rcp/co2 combination formatted like this:
#' "yields:EPIC-IIASA:ukesm1-0-ll:ssp585:default:3b"
#' @param climatetype Switch between different climate scenarios
#' @param cells if cellular is TRUE: "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @param weighting use of different weights (totalCrop (default), totalLUspecific, cropSpecific, crop+irrigSpecific,
#'                                            avlCropland, avlCropland+avlPasture)
#' @param indiaYields if TRUE returns scaled yields for rainfed crops in India
#' @param scaleFactor integer value by which indiaYields will be scaled
#' @return magpie object in cellular resolution
#' @author Kristine Karstens, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("Yields", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset
#' @importFrom magclass getYears add_columns dimSums time_interpolate
#' @importFrom madrat toolFillYears

calcYields <- function(source = c(lpjml = "ggcmi_phase3_nchecks_9ca735cb", isimip = NULL),
                       climatetype = "GSWP3-W5E5:historical", cells = "magpiecell",
                       weighting = "totalCrop", indiaYields = FALSE, scaleFactor = 0.3) {

  cfg <- toolLPJmLVersion(version = source["lpjml"], climatetype = climatetype)

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit = 1e+12)
  on.exit(options(magclass_sizeLimit = sizelimit))

  if (grepl("GSWP3-W5E5", climatetype)) {
    stage       <- "smoothed"
    climatetype <- cfg$baseline_hist
  } else {
    stage <- "harmonized2020"
  }

  LPJ2MAG      <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")
  lpjml_crops  <- unique(LPJ2MAG$LPJmL)
  irrig_types  <- c("irrigated", "rainfed")
  yields       <- list()

  for (crop in lpjml_crops) {
    subdata <- as.vector(outer(crop, irrig_types, paste, sep = "."))
    yields[[crop]] <- calcOutput("LPJmL_new", version = source[["lpjml"]], climatetype = climatetype,
                                 subtype = "harvest", subdata = subdata, stage = stage, aggregate = FALSE)
  }
  yields  <- mbind(yields)

  # include garbage collector to solve recurring memory problems
  gc()

  # LPJmL to MAgPIE crops
  yields <- toolAggregate(yields, LPJ2MAG, from = "LPJmL", to = "MAgPIE", dim = 3.1, partrel = TRUE)

  # Check for NAs
  if (any(is.na(yields))) {
    stop("produced NA yields")
  }

  # Use FAO data to scale proxy crops to reasonable levels (global, static factor)
  FAOproduction <- collapseNames(calcOutput("FAOmassbalance_pre", aggregate = FALSE)[, , "production"][, , "dm"])
  MAGarea       <- calcOutput("Croparea", sectoral = "kcr", physical = TRUE, aggregate = FALSE)
  faoyears      <- intersect(getYears(FAOproduction), getYears(MAGarea))

  MAGcroptypes  <- findset("kcr")
  missing       <- c("betr", "begr")
  MAGcroptypes  <- setdiff(MAGcroptypes, missing)
  FAOproduction <- add_columns(FAOproduction[, , MAGcroptypes], addnm = missing, dim = 3.1)
  FAOproduction[, , missing] <- 0

  FAOYields     <- dimSums(FAOproduction[, faoyears, ], dim = 1) / dimSums(MAGarea[, faoyears, ], dim = 1)
  FAOYields     <- setYears(toolTimeAverage(FAOYields[, 1993:1997, ], 5))
  Calib         <- new.magpie(cells_and_regions = "GLO",
                              years = NULL,
                              names = c(getNames(FAOYields), "pasture"),
                              fill = 1,
                              sets = c("iso", "year", "data"))
  Calib[, , "oilpalm"]   <- FAOYields[, , "oilpalm"] / FAOYields[, , "groundnut"]  # LPJmL proxy for oilpalm is groundnut
  Calib[, , "cottn_pro"] <- FAOYields[, , "cottn_pro"] / FAOYields[, , "groundnut"] # LPJmL proxy for cotton is groundnut
  Calib[, , "foddr"]     <- FAOYields[, , "foddr"] / FAOYields[, , "maiz"]         # LPJmL proxy for fodder is maize
  Calib[, , "others"]    <- FAOYields[, , "others"] / FAOYields[, , "maiz"]        # LPJmL proxy for others is maize
  Calib[, , "potato"]    <- FAOYields[, , "potato"] / FAOYields[, , "sugr_beet"]   # LPJmL proxy for potato is sugarbeet

  # recalibrate yields for proxys
  yields <- yields * Calib[, , getNames(yields, dim = 1)]

  if (cells == "magpiecell") {
    yields <- toolCoord2Isocell(yields)
  }

  if (!is.na(source["isimip"])) {
    to_rep <- calcOutput("ISIMIP3bYields", subtype = source[["isimip"]], cells = cells, aggregate = FALSE)
    commonVars  <- intersect(getNames(yields), getNames(to_rep))
    commonYears <- intersect(getYears(yields), getYears(to_rep))

    #  harmonize to LPJml
    cfg <- toolLPJmLVersion(version = source["lpjml"], climatetype = climatetype)
    harm_rep <- toolHarmonize2Baseline(x = to_rep[, commonYears, commonVars],
                                       base = yields[, commonYears, commonVars],
                                       ref_year = cfg$ref_year_gcm)
    gc()
    # convert to array for memory
    yields <- as.array(yields)
    harm_rep <- as.array(harm_rep)
    # yields[,commonYears,commonVars] <- ifelse(to_rep[,commonYears,commonVars] >0, to_rep[,commonYears,commonVars], yields[,commonYears, commonVars])
    yields[, commonYears, commonVars] <- harm_rep[, commonYears, commonVars]
    yields <- as.magpie(yields)
    harm_rep <- as.magpie(harm_rep)

  }

  if (weighting == "totalCrop") {

    cropAreaWeight <- dimSums(calcOutput("Croparea", sectoral = "kcr", physical = TRUE, irrigation = FALSE,
                                           cellular = TRUE, cells = cells, aggregate = FALSE,
                                           years = "y1995", round = 6), dim = 3)

  } else if (weighting %in% c("totalLUspecific", "cropSpecific", "crop+irrigSpecific")) {

    crop <- calcOutput("Croparea", sectoral = "kcr", physical = TRUE, irrigation = TRUE,
                       cellular = TRUE, cells = cells, aggregate = FALSE, years = "y1995", round = 6)

    past <- calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE, nclasses = "seven", fao_corr = TRUE,
                       input_magpie = TRUE, cells = cells, years = "y1995", round = 6)[, , "past"]

    if (weighting == "crop+irrigSpecific") {

      cropAreaWeight <- new.magpie(cells_and_regions = getCells(yields), 
                                   years = NULL,
                                   names = getNames(yields), 
                                   fill = NA)
      cropAreaWeight[, , findset("kcr")] <- crop + 10^-10
      cropAreaWeight[, , "pasture"]      <- mbind(setNames(past + 10^-10, "irrigated"),
                                                    setNames(past + 10^-10, "rainfed"))

    } else if (weighting == "cropSpecific") {

      cropAreaWeight <- new.magpie(cells_and_regions = getCells(yields),
                                   years = NULL,
                                   names = getNames(yields, dim = 1),
                                   fill = NA)

      cropAreaWeight[, , findset("kcr")] <- dimSums(crop, dim = 3.1) + 10^-10
      cropAreaWeight[, , "pasture"]      <- past + 10^-10

    } else {

      cropAreaWeight <- new.magpie(cells_and_regions = getCells(yields),
                                   years = NULL,
                                   names = getNames(yields, dim = 1),
                                   fill = (dimSums(crop, dim = 3) + 10^-10))

      cropAreaWeight[, , "pasture"] <- past + 10^-10

    }

  } else if (weighting == "avlCropland") {

    cropAreaWeight <- setNames(calcOutput("AvlCropland", marginal_land = "all_marginal", cells = cells,
                                            country_level = FALSE, aggregate = FALSE),
                               NULL)

  } else if (weighting == "avlCropland+avlPasture") {

    avlCrop <- setNames(calcOutput("AvlCropland", marginal_land = "all_marginal", cells = cells,
                                   country_level = FALSE, aggregate = FALSE), "avlCrop")

    lu1995  <- setYears(calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE, nclasses = "seven",
                                   fao_corr = TRUE, input_magpie = TRUE, cells = cells,
                                   years = "y1995", round = 6), NULL)

    cropAreaWeight <- new.magpie(cells_and_regions = getCells(yields),
                                 years = NULL,
                                 names = getNames(yields, dim = 1),
                                 fill = avlCrop)

    cropAreaWeight[, , "pasture"] <- pmax(avlCrop,
                                           dimSums(lu1995[, , c("primforest", "secdforest", "forestry", "past")],
                                                   dim = 3))

  } else {

    stop("Weighting setting is not available.")
  }

  if (any(is.na(cropAreaWeight))) stop("NAs in weights.")

  if (cells == "lpjcell") {
    cropAreaWeight <- addLocation(cropAreaWeight)
  }

  if (indiaYields) {
    yields["IND", , "rainfed"] <- yields["IND", , "rainfed"] * scaleFactor
  }


  return(list(x            = yields,
              weight       = cropAreaWeight,
              unit         = "t per ha",
              description  = "Yields in tons per hectar for different crop types.",
              isocountries = FALSE))
}
