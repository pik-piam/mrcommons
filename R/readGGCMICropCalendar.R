#' @title readGGCMICropCalendar
#' @description Reads in GGCMI fraction of Harvested Area masks for rice 1 and rice 2
#'  (other crops available too, see path in download function), or other variables available in
#'  the GGCMI crop calendar.
#' @param subtype variable or vector of variables to read from the crop calendar set. Options:
#' ("planting_day","maturity_day","fraction_of_harvested_area","cal" (which is a combination of planting_day,
#' maturity_day and harvest_day)), wheat areas and rice_areas
#' @return MAgPIE object with the requested data
#' @author David M Chen, Edna Molina Bacca
#'
#' @importFrom raster raster
#' @importFrom terra rast subset
#'
readGGCMICropCalendar <- function(subtype = "fraction_of_harvested_area") {

  if (subtype %in% c("fraction_of_harvested_area", "wheat_areas", "rice_areas")) {
    x <- toolReadArea(subtype)
  } else if (subtype == "cal") {
    x <- toolReadCal()
  } else {
    x <- NULL
    for (crop in c("bar", "bea", "cas", "cot", "mai", "mil",
                   "nut", "pea", "pot", "rap", "ri1", "ri2", "rye",
                   "sgb", "sgc", "sor", "soy", "sun", "swh", "wwh")) {
      for (irr in c("ir", "rf")) {
        mask <- rast(paste0(crop, "_", irr, "_ggcmi_crop_calendar_phase3_v1.01.nc4"))
        mask <- subset(mask, subtype)
        mag <- as.magpie(raster(mask))
        getNames(mag) <- crop
        missingCells <- setdiff(getItems(x, dim = 1), getItems(mag, dim = 1))

        if (length(missingCells) > 0) {
          fill <- new.magpie(cells_and_regions = missingCells, years = getYears(mag),
                             names = getNames(mag), fill = NA)
          mag <- mbind(mag, fill)
        }
        mag <- add_dimension(mag, dim = 3.2, add = "irr", nm = irr)
        mag <- add_dimension(mag, dim = 3.3, add = "var", nm = subtype)
        x <- mbind(x, mag)
      }
    }
  }

  return(x)
}

toolReadArea <- function(subtype) {
  x <- NULL
  for (crop in c("ri1", "ri2")) {
    for (irr in c("ir", "rf")) {

      mask <- rast(paste0(crop, "_", irr, "_ggcmi_crop_calendar_phase3_v1.01.nc4"))
      mask <- subset(mask, "fraction_of_harvested_area")
      mag <- as.magpie(raster(mask))
      getNames(mag) <- crop
      mag <- add_dimension(mag, dim = 3.2, add = "irr", nm = irr)
      mag <- add_dimension(mag, dim = 3.3, add = "var", nm = subtype)
      x <- mbind(x, mag)
    }
  }

  # Wheat
  wheatMasks <- rast("winter_and_spring_wheat_areas_phase3.nc4")
  swh <- subset(wheatMasks, "swh_mask")
  wwh <- subset(wheatMasks, "wwh_mask")

  swh <- as.magpie(raster(swh))
  wwh <- as.magpie(raster(wwh))

  wheat <- mbind(swh, wwh)
  getNames(wheat) <- gsub("_mask", "", getNames(wheat))
  wheat <- add_dimension(wheat, dim = 3.2, add = "irr", nm = c("ir", "rf"))
  # fill missing cells in wheat
  missingCells <- setdiff(getItems(x, dim = 1), getItems(wheat, dim = 1))
  fill <- new.magpie(cells_and_regions = missingCells, years = getYears(wheat), names = getNames(wheat),  fill = 0)
  wheat <- mbind(wheat, fill)
  wheat <- add_dimension(wheat, dim = 3.3, add = "var", nm = subtype)
  x <- mbind(x, wheat)

  if (subtype == "wheat_areas") {
    x <- toolCoord2Isocell(collapseNames(x[, , c("wwh", "swh")]))
  } else if (subtype == "rice_areas") {
    x <- toolCoord2Isocell(collapseNames(x[, , c("ri1", "ri2")][, , "ir"]))
  }

  return(x)
}

toolReadCal <- function() {
  x <- NULL
  for (crop in c("bar", "bea", "cas", "cot", "mai", "mil",
                 "nut", "pea", "pot", "rap", "ri1", "ri2", "rye",
                 "sgb", "sgc", "sor", "soy", "sun", "swh", "wwh")) {
    for (irr in c("ir", "rf")) {
      for (subtype2 in c("planting_day", "maturity_day")) {
        mask <- rast(paste0(crop, "_", irr, "_ggcmi_crop_calendar_phase3_v1.01.nc4"))
        mask <- subset(mask, subtype2)
        mag <- as.magpie(raster(mask))
        getNames(mag) <- crop
        missingCells <- setdiff(getItems(x, dim = 1), getItems(mag, dim = 1))

        if (length(missingCells) > 0) {
          fill <- new.magpie(cells_and_regions = missingCells, years = getYears(mag),
                             names = getNames(mag), fill = NA)
          mag <- mbind(mag, fill)
        }
        mag <- add_dimension(mag, dim = 3.2, add = "irr", nm = irr)
        mag <- add_dimension(mag, dim = 3.3, add = "var", nm = subtype2)
        x <- mbind(x, mag)
      }
    }
  }

  x[is.na(x)] <- 0

  ## some crops are left for some days after maturity until harvest, Elliott et al. 2015source data from
  ## pdf in source folder
  daysToHarvest <- new.magpie(cells_and_regions = getItems(x, dim = 1),
                              years = NULL, names = getItems(x, dim = 3.1), fill = 0)
  daysToHarvest[, , c("mai", "soy")] <- 21
  daysToHarvest[, , c("swh", "wwh", "bar", "rye", "rap")] <- 7

  x <- magclass::add_columns(x, addnm = "harvest_day", dim = 3.3, fill = 0)
  x[, , "harvest_day"] <- x[, , "maturity_day"] + daysToHarvest

  x <- toolCoord2Isocell(x)
  return(x)
}
