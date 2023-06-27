#' @title calcProduction
#' @description Distributes crop, pasture and livestock production in space to 0.5 degree
#'
#' @param products   setname of products ("kcr", "kli", "pasture")
#' @param cellular   if TRUE production is calculate on cellular level
#' @param cells      Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param calibrated if FALSE, lpj yields will be used uncalibrated,
#'                   if true, calibrated on FAP production on country level
#' @param attributes "All" for all crop attributes, or specify e.g. DM (dry matter), Nr (nitrogen) for memory reduction
#' @param irrigation if TRUE, additional information on irrigated and rainfed production is provided
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcLanduseInitialisation()],
#' [calcCroparea()]
#' @examples
#' \dontrun{
#' calcOutput("Production")
#' }
#'
#' @importFrom magclass getSets magpie_expand new.magpie getCPR
#' @importFrom magpiesets findset


calcProduction <- function(products = "kcr", cellular = FALSE, # nolint
                           cells = "lpjcell", calibrated = TRUE,
                           attributes = "all", irrigation = FALSE) {

  selectyears <- findset("past")

  if (products == "kcr") {

    magCropTypes  <- findset("kcr")
    missing       <- c("betr", "begr")
    magCropTypes  <- setdiff(magCropTypes, missing)

    if (!cellular) {
      if (irrigation) {
        stop("Irrigation not yet implemented for this resolution")
      }
      productionMAG <- collapseNames(calcOutput("FAOmassbalance_pre",
                                                aggregate = FALSE)[, , "production"][, , magCropTypes])
      productionMAG <- add_columns(productionMAG, addnm = missing, dim = 3.1)
      productionMAG[, , missing] <- 0

    } else {
      ################################
      ### crop production cellular ###
      ################################
      yieldsLPJ      <- calcOutput("LPJmL_new", version = "ggcmi_phase3_nchecks_9ca735cb",
                                   climatetype = "GSWP3-W5E5:historical", subtype = "harvest",
                                   stage = "smoothed", aggregate = FALSE)[, selectyears, ]

      mappingCountryCell <- toolGetMappingCoord2Country()
      mappingCountryCell$coordiso <- paste(mappingCountryCell$coords, mappingCountryCell$iso, sep = ".")

      # crop mapping from LPJmL to MAgPIE categories
      mappingMAG2LPJ <- toolGetMapping(type = "sectoral", name = "MAgPIE_LPJmL.csv")
      mappingMAG2LPJ <- mappingMAG2LPJ[which(mappingMAG2LPJ$MAgPIE %in% magCropTypes), ]
      yieldsMAG      <- toolAggregate(x = yieldsLPJ, rel = mappingMAG2LPJ, from = "LPJmL", to = "MAgPIE",
                                      dim = 3.1, partrel = TRUE)[, , magCropTypes]

      cropareaMAG    <- calcOutput("Croparea", sectoral = "kcr", physical = TRUE,
                                    cellular = TRUE, cells = "lpjcell",
                                    irrigation = TRUE, aggregate = FALSE)[, selectyears, magCropTypes]

      if (calibrated == TRUE) {

        tau       <- calcOutput("LanduseIntensity", sectoral = "kcr", rescale = FALSE,
                                 aggregate = FALSE)[, selectyears, magCropTypes]
        tauCell   <- toolAggregate(x = tau, rel = mappingCountryCell,
                                   from = "iso", to = "coordiso", partrel = TRUE)
        getSets(tauCell) <- c("x", "y", "iso", "year", "ItemCodeItem")

        yieldsMAG <- tauCell * yieldsMAG
      }

      productionMAG  <- yieldsMAG * cropareaMAG

      #####################################################################
      # correct production mismatch - generic approach

      isoproductionMAG  <- isoMismatch <- dimSums(productionMAG, dim = c(1.1, 1.2, 3.2))
      countries         <- getItems(isoproductionMAG, dim = 1)
      prods             <- getNames(productionMAG, dim = 1)
      productionFAO     <- calcOutput("FAOmassbalance",
                                      aggregate = FALSE)[, selectyears, "production.dm"][countries, , prods]
      productionFAO     <- collapseNames(productionFAO)
      isoMismatch[, , ] <- abs(round(isoproductionMAG - productionFAO, 4)) > 0

      if (any(isoMismatch != 0)) {
        # correct items with no crop-specific area
        isoMAGCroparea   <- noMAGCroparea <- dimSums(cropareaMAG, dim = c("x", "y"))
        noMAGCroparea[]  <- (isoMAGCroparea == 0) * isoMismatch

        if (any(noMAGCroparea != 0)) {
          # distribute total cropland weighted over all non-irrigated cells first
          productionMAG[, , "rainfed"] <- productionMAG[, , "rainfed"] * (1 - noMAGCroparea[, , "rainfed"]) +
            toolAggregate(noMAGCroparea[, , "rainfed"] * (productionFAO - isoproductionMAG),
                          rel = mappingCountryCell,
                          weight = dimSums(cropareaMAG[, , "rainfed"], dim = 3),
                          from = "iso", to = "coordiso")

          isoproductionMAG  <- dimSums(productionMAG, dim = c(1.1, 1.2, 3.2))

          # distribute total cropland weighted over irrigated cells
          productionMAG[, , "irrigated"] <- productionMAG[, , "irrigated"] * (1 - noMAGCroparea[, , "irrigated"]) +
            toolAggregate(noMAGCroparea[, , "irrigated"] * (productionFAO - isoproductionMAG),
                          rel = mappingCountryCell,
                          weight = dimSums(cropareaMAG[, , "irrigated"], dim = 3),
                          from = "iso", to = "coordiso")
        }

        # correct items with no total cropland area for known data mismatches:
        # where no LUH croparea at all, but FAO production reported
        # Note: Number of countries with mismatch would reduce if we use the croparea of LandInG
        isoMAGTotCrop  <- dimSums(isoMAGCroparea, dim = 3)
        noMAGTotCrop   <- (isoMAGTotCrop == 0) * isoMismatch

        if (any(noMAGTotCrop["MUS", , ] != 0)) {
          productionMAG["MUS", , "rainfed"] <- productionFAO["MUS", , ] /
            length(getItems(productionMAG["MUS", , ], dim = 1))
        }
        if (any(noMAGTotCrop["KIR", , ] != 0)) {
           productionMAG["KIR", , "rainfed"]  <- productionFAO["KIR", , ] /
                                                  length(getItems(productionMAG["KIR", , ], dim = 1))
        }
        if (any(noMAGTotCrop["PYF", , ] != 0)) {
          productionMAG["PYF", , "rainfed"] <- productionFAO["PYF", , ] /
                                                  length(getItems(productionMAG["PYF", , ], dim = 1))
        }
        if (any(noMAGTotCrop["BMU", , ] != 0)) {
          productionMAG["BMU", , "rainfed"] <- productionFAO["BMU", , ] /
                                                  length(getItems(productionMAG["BMU", , ], dim = 1))
        }
        if (any(noMAGTotCrop["KNA", , ] != 0)) {
          productionMAG["KNA", , "rainfed"] <- productionFAO["KNA", , ] /
                                                  length(getItems(productionMAG["KNA", , ], dim = 1))
        }
        if (any(noMAGTotCrop["GRD", , ] != 0)) {
          productionMAG["GRD", , "rainfed"] <- productionFAO["GRD", , ] /
                                                  length(getItems(productionMAG["GRD", , ], dim = 1))
        }
        if (any(noMAGTotCrop["ATG", , ] != 0)) {
          productionMAG["ATG", , "rainfed"] <- productionFAO["ATG", , ] /
                                                  length(getItems(productionMAG["ATG", , ], dim = 1))
        }
        if (any(noMAGTotCrop["DMA", , ] != 0)) {
          productionMAG["DMA", , "rainfed"] <- productionFAO["DMA", , ] /
                                                  length(getItems(productionMAG["DMA", , ], dim = 1))
        }
        if (any(noMAGTotCrop["LCA", , ] != 0)) {
          productionMAG["LCA", , "rainfed"] <- productionFAO["LCA", , ] /
                                                  length(getItems(productionMAG["LCA", , ], dim = 1))
        }
        if (any(noMAGTotCrop["BRB", , ] != 0)) {
          productionMAG["BRB", , "rainfed"] <- productionFAO["BRB", , ] /
                                                  length(getItems(productionMAG["BRB", , ], dim = 1))
        }
        if (any(noMAGTotCrop["STP", , ] != 0)) {
          productionMAG["STP", , "rainfed"] <- productionFAO["STP", , ] /
                                                  length(getItems(productionMAG["STP", , ], dim = 1))
        }
        if (any(noMAGTotCrop["MDV", , ] != 0)) {
          productionMAG["MDV", , "rainfed"] <- productionFAO["MDV", , ] /
                                                  length(getItems(productionMAG["MDV", , ], dim = 1))
        }
        if (any(noMAGTotCrop["HKG", , ] != 0)) {
          productionMAG["HKG", , "rainfed"] <- productionFAO["HKG", , ] /
                                                  length(getItems(productionMAG["HKG", , ], dim = 1))
        }
        if (any(noMAGTotCrop["MLT", , ] != 0)) {
          productionMAG["MLT", , "rainfed"] <- productionFAO["MLT", , ] /
                                                  length(getItems(productionMAG["MLT", , ], dim = 1))
        }
        if (any(noMAGTotCrop["BHR", , ] != 0)) {
          productionMAG["BHR", , "rainfed"] <- productionFAO["BHR", , ] /
                                                  length(getItems(productionMAG["BHR", , ], dim = 1))
        }

        # correct items with no yields
        isoMAGYields   <- noMAGYields <- toolAggregate(yieldsMAG, weight = cropareaMAG,
                                                       rel = mappingCountryCell,
                                                       from = "coordiso", to = "iso")
        noMAGYields[]  <- (isoMAGYields == 0) * isoMismatch * (1 - noMAGCroparea)

        if (any(noMAGYields != 0)) {

          isoproductionMAG  <- dimSums(productionMAG, dim = c(1.1, 1.2, 3.2))

          # distribute corresponding to crop area share
          productionMAG[, , "rainfed"]   <- productionMAG[, , "rainfed"] * (1 - noMAGYields[, , "rainfed"]) +
            noMAGYields[, , "rainfed"] * toolAggregate(productionFAO - isoproductionMAG,
                                                       rel = mappingCountryCell,
                                                       weight = cropareaMAG[, , "rainfed"],
                                                       from = "iso", to = "coordiso")

          isoproductionMAG  <- dimSums(productionMAG, dim = c(1.1, 1.2, 3.2))

          # distribute corresponding to crop area share
          productionMAG[, , "irrigated"]   <- productionMAG[, , "irrigated"] * (1 - noMAGYields[, , "irrigated"]) +
            noMAGYields[, , "irrigated"] * toolAggregate(productionFAO - isoproductionMAG,
                                                         rel = mappingCountryCell,
                                                         weight = cropareaMAG[, , "irrigated"],
                                                         from = "iso", to = "coordiso")
        }
      }

      isoproductionMAG  <- isoMismatch <- dimSums(productionMAG, dim = c(1.1, 1.2, 3.2))
      isoMismatch[]     <- abs(round(isoproductionMAG - productionFAO, 4)) > 0

      if (any(isoMismatch != 0)) {
        warning(paste0("Cellular data to FAO production mismatch ",
                       "after generic fix in calcProduction. Please check!"))
      }

      #####################################################################
      if (!irrigation) productionMAG  <- dimSums(productionMAG, dim = 3.2)

      prodAttributes <- calcOutput("Attributes", aggregate = FALSE)[, , magCropTypes]

      if (any(attributes != "all")) {
        prodAttributes <- prodAttributes[, , attributes]
      }

      productionMAG  <- productionMAG * prodAttributes
      productionMAG  <- add_columns(productionMAG, addnm = missing, dim = 3.1)
      productionMAG[, , missing] <- 0
    }

  } else if (products == "pasture") {
    if (irrigation) {
      stop("Irrigation not yet implemented for this Product group")
    }
    if (!cellular) {

      productionMAG  <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, , "pasture.production"])

    } else {
      ####################################
      ### pasture production celluluar ###
      ####################################

      areaPasture    <- collapseNames(calcOutput("LanduseInitialisation",
                                                cellular = TRUE, cells = "lpjcell",
                                                aggregate = FALSE)[, selectyears, "past"])
      yieldsPasture  <- collapseNames(calcOutput("LPJmL_new", version = "ggcmi_phase3_nchecks_9ca735cb",
                                      climatetype = "GSWP3-W5E5:historical", subtype = "harvest", stage = "smoothed",
                                      aggregate = FALSE, years = selectyears)[, , "mgrass.rainfed"])

      mappingCountryCell <- toolGetMappingCoord2Country()
      mappingCountryCell$coordiso <- paste(mappingCountryCell$coords, mappingCountryCell$iso, sep = ".")

      if (calibrated == TRUE) {

        tau           <- calcOutput("LanduseIntensity", sectoral = "pasture", rescale = FALSE,
                                     aggregate = FALSE)[, selectyears, ]
        tauCell       <- toolAggregate(x = tau, rel = mappingCountryCell,
                                       from = "iso", to = "coordiso", partrel = TRUE)

        yieldsPasture <- tauCell * yieldsPasture
      }

      productionMAG    <- yieldsPasture * areaPasture

      #####################################################################
      # correct production mismatch - generic approach

      isoproductionMAG  <- isoMismatch <- dimSums(productionMAG, dim = c(1.1, 1.2))
      countries         <- getItems(isoproductionMAG, dim = 1)
      productionFAO     <- collapseNames(calcOutput("FAOmassbalance",
                                                    aggregate = FALSE)[countries, selectyears, "pasture.production.dm"])

      isoMismatch[]     <- abs(round(isoproductionMAG - productionFAO, 4)) > 0

      if (any(isoMismatch != 0)) {
        # correct items with no area
        isoPastureArea   <- noPastureArea <- dimSums(areaPasture, dim = c("x", "y"))
        noPastureArea[]  <- (isoPastureArea == 0) * isoMismatch

        if (any(noPastureArea != 0)) {
          # distribute equally over all cells
          productionMAG     <- productionMAG * (1 - noPastureArea) +
            magpie_expand(noPastureArea * productionFAO, productionMAG) /
            new.magpie(names(getCPR(productionMAG, dim = 1.3)), fill = getCPR(productionMAG, dim = 1.3))
        }

        # correct items with no yields
        isoPastureYields   <- noPastureYields <- toolAggregate(yieldsPasture, weight = areaPasture,
                                                               rel = mappingCountryCell, from = "coordiso", to = "iso")
        noPastureYields[]  <- (isoPastureYields == 0) * isoMismatch * (1 - noPastureArea)

        if (any(noPastureYields != 0)) {
          # distribute corresponding to pasture area share
          productionMAG     <- productionMAG * (1 - noPastureYields) +
            noPastureYields * toolAggregate(toolIso2CellCountries(productionFAO), rel = mappingCountryCell,
                                            weight = areaPasture, from = "iso", to = "coordiso")
        }
      }

      isoproductionMAG  <- isoMismatch <- dimSums(productionMAG, dim = c(1.1, 1.2))
      isoMismatch[]     <- abs(round(isoproductionMAG - productionFAO, 4)) > 0

      if (any(isoMismatch != 0)) warning("Cellular data to FAO production mismatch after generic fix. Please check.")

      prodAttributes <- calcOutput("Attributes", aggregate = FALSE)[, , "pasture"]
      productionMAG  <- collapseNames(productionMAG * prodAttributes)

    }

  } else if (products == "kli") {

    livestockTypes <- findset("kli")
    if (irrigation) {
      stop("Irrigation not yet implemented for this Product group")
    }
    if (!cellular) {
      productionMAG <- collapseNames(calcOutput("FAOmassbalance_pre",
                                                aggregate = FALSE)[, , livestockTypes][, , "production"])
    } else {
      productionMAG <- calcOutput("LivestockGridded", aggregate = FALSE)
    }

  } else {
    stop("Products so far can only be kcr,kli,or pasture")
  }

  x <- productionMAG

  if (any(attributes != "all")) {
    x <- x[, , attributes]
  }

  if (cellular) {
    if (cells == "magpiecell") {
      x <- toolCoord2Isocell(x, cells = cells)
    }
  }

  # Check for NAs and negatives
  if (any(round(x, digits = 4) < 0)) {
    stop("calcProduction produced negative values")
  }
  if (any(is.na(x))) {
    stop("calcProduction produced NA values")
  }

  return(list(x = x,
              weight = NULL,
              unit = "Mt DM/Nr/P/K/WM or PJ energy",
              description = "Crop, pasture and livestock production:
              dry matter: Mt (dm), gross energy: PJ (ge), reactive nitrogen: Mt (nr),
              phosphor: Mt (p), potash: Mt (k), wet matter: Mt (wm).",
              min = -Inf,
              max = Inf,
              isocountries = !cellular))
}
