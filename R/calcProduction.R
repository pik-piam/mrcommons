#' @title calcProduction
#' @description Distributes crop, pasture and livestock production in space to 0.5 degree
#'
#' @param products setname of products ("kcr", "kli", "pasture")
#' @param cellular if TRUE production is calculate on cellular level
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


calcProduction <- function(products = "kcr", cellular = FALSE, calibrated = TRUE,
                           attributes = "all", irrigation = FALSE) {

  selectyears <- findset("past")

  if (products == "kcr") {

    MAGcroptypes  <- findset("kcr")
    missing       <- c("betr", "begr")
    MAGcroptypes  <- setdiff(MAGcroptypes, missing)

    if (!cellular) {
      if (irrigation) {
        stop("Irrigation not yet implemented for this resolution")
      }
      productionMAG <- collapseNames(calcOutput("FAOmassbalance_pre",
                                                aggregate = FALSE)[, , "production"][, , MAGcroptypes])
      productionMAG <- add_columns(productionMAG, addnm = missing, dim = 3.1)
      productionMAG[, , missing] <- 0

    } else {

      ################################
      ### crop production cellular ###
      ################################

      yieldsLPJ      <- calcOutput("LPJmL_new", version = "ggcmi_phase3_nchecks_9ca735cb",
                                   climatetype = "GSWP3-W5E5:historical", subtype = "harvest",
                                   stage = "smoothed", aggregate = FALSE)[, selectyears, ]
      # reduce to 59199 cells and rename
      yieldsLPJ      <- toolCoord2Isocell(yieldsLPJ)

      mappingCountryCell <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")
      mappingMAG2LPJ     <- toolGetMapping(type = "sectoral", name = "MAgPIE_LPJmL.csv")
      mappingMAG2LPJ     <- mappingMAG2LPJ[which(mappingMAG2LPJ$MAgPIE %in% MAGcroptypes), ]

      yieldsMAG      <- toolAggregate(x = yieldsLPJ, rel = mappingMAG2LPJ, from = "LPJmL", to = "MAgPIE",
                                      dim = 3.1, partrel = TRUE)
      cropareaMAG    <- toolCell2isoCell(
                          calcOutput("Croparea", sectoral = "kcr", physical = TRUE, cellular = TRUE,
                                     irrigation = TRUE, aggregate = FALSE)[, selectyears, MAGcroptypes])

      if (calibrated == TRUE) {

        tau            <- calcOutput("LanduseIntensity", sectoral = "kcr", rescale = FALSE,
                                     aggregate = FALSE)[, selectyears, MAGcroptypes]
        tauCell        <- toolAggregate(x = tau, rel = mappingCountryCell, from = "iso", to = "celliso", partrel = TRUE)

        yieldsMAG      <- tauCell * yieldsMAG
      }

      productionMAG  <- yieldsMAG * cropareaMAG

      #####################################################################
      # correct production mismatch - generic approach

      isoproductionMAG  <- isoMismatch <- toolAggregate(dimSums(productionMAG, dim = 3.2),
                                                        rel = mappingCountryCell, from = "celliso", to = "iso")
      productionFAO     <- collapseNames(calcOutput("FAOmassbalance",
                                                    aggregate = FALSE)[, selectyears, "production"][, , getNames(productionMAG, dim = 1)][, , "dm"])
      isoMismatch[]     <- abs(round(isoproductionMAG - toolIso2CellCountries(productionFAO), 4)) > 0

      if (any(isoMismatch != 0)) {

        # correct items with no cropspecific area
        isoMAGCroparea   <- noMAGCroparea <- toolAggregate(cropareaMAG, rel = mappingCountryCell,
                                                           from = "celliso", to = "iso")
        noMAGCroparea[]  <- (isoMAGCroparea == 0) * isoMismatch

        if (any(noMAGCroparea != 0)) {

          # distribute total cropland weighted over all non-irrigated cells first
          productionMAG[, , "rainfed"]  <- productionMAG[, , "rainfed"] * (1 - noMAGCroparea[, , "rainfed"]) +
            toolAggregate(noMAGCroparea[, , "rainfed"] * (toolIso2CellCountries(productionFAO) - isoproductionMAG),
                          rel = mappingCountryCell, weight = dimSums(cropareaMAG[, , "rainfed"], dim = 3),
                          from = "iso", to = "celliso")

          isoproductionMAG  <- toolAggregate(dimSums(productionMAG, dim = 3.2), rel = mappingCountryCell,
                                             from = "celliso", to = "iso")

          # distribute total cropland weighted over irrigated cells
          productionMAG[, , "irrigated"]   <- productionMAG[, , "irrigated"] * (1 - noMAGCroparea[, , "irrigated"]) +
            toolAggregate(noMAGCroparea[, , "irrigated"] * (toolIso2CellCountries(productionFAO) - isoproductionMAG),
                          rel = mappingCountryCell, weight = dimSums(cropareaMAG[, , "irrigated"], dim = 3),
                          from = "iso", to = "celliso")
        }

        # correct item with no total cropland area for known data mismatches - so far known:
        # * MUS (Mauritius) as non align LUH (no area at all) and FAO data

        isoMAGTotCrop  <- dimSums(isoMAGCroparea, dim = 3)
        noMAGTotCrop   <- (isoMAGTotCrop == 0) * isoMismatch

        if (any(noMAGTotCrop["MUS", , ] != 0)) {

           productionMAG["MUS", , "rainfed"]  <- productionFAO["MUS", , ]
        }

        # correct items with no yields
        isoMAGYields   <- noMAGYields <- toolAggregate(yieldsMAG, weight = cropareaMAG, rel = mappingCountryCell,
                                                       from = "celliso", to = "iso")
        noMAGYields[]  <- (isoMAGYields == 0) * isoMismatch * (1 - noMAGCroparea)

        if (any(noMAGYields != 0)) {

          isoproductionMAG  <- toolAggregate(dimSums(productionMAG, dim = 3.2), rel = mappingCountryCell,
                                             from = "celliso", to = "iso")

          # distribute corresponding to crop area share
          productionMAG[, , "rainfed"]   <- productionMAG[, , "rainfed"] * (1 - noMAGYields[, , "rainfed"]) +
            noMAGYields[, , "rainfed"] * toolAggregate(toolIso2CellCountries(productionFAO) - isoproductionMAG,
                                                       rel = mappingCountryCell, weight = cropareaMAG[, , "rainfed"],
                                                       from = "iso", to = "celliso")

          isoproductionMAG  <- toolAggregate(dimSums(productionMAG, dim = 3.2), rel = mappingCountryCell,
                                             from = "celliso", to = "iso")

          # distribute corresponding to crop area share
          productionMAG[, , "irrigated"]   <- productionMAG[, , "irrigated"] * (1 - noMAGYields[, , "irrigated"]) +
            noMAGYields[, , "irrigated"] * toolAggregate(toolIso2CellCountries(productionFAO) - isoproductionMAG,
                                                         rel = mappingCountryCell, weight = cropareaMAG[, , "irrigated"],
                                                         from = "iso", to = "celliso")
        }

      }

      isoproductionMAG  <- isoMismatch <- toolAggregate(dimSums(productionMAG, dim = 3.2), rel = mappingCountryCell,
                                                        from = "celliso", to = "iso")
      isoMismatch[]     <- abs(round(isoproductionMAG - toolIso2CellCountries(productionFAO), 4)) > 0

      if (any(isoMismatch != 0)) warning("Cellular data to FAO production mismatch after generic fix. Please check.")
      #####################################################################

      if (!irrigation) productionMAG  <- dimSums(productionMAG, dim = 3.2)

      prodAttributes <- calcOutput("Attributes", aggregate = FALSE)[, , MAGcroptypes]

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

      productionMAG  <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, , "production"][, , "pasture"])

    } else {

      ####################################
      ### pasture production celluluar ###
      ####################################

      areaPasture    <- toolCell2isoCell(collapseNames(calcOutput("LanduseInitialisation", cellular = TRUE,
                                                                  aggregate = FALSE)[, selectyears, "past"]))
      yieldsPasture  <- toolCoord2Isocell(
                          collapseNames(
                            calcOutput("LPJmL_new", version = "ggcmi_phase3_nchecks_9ca735cb",
                                      climatetype = "GSWP3-W5E5:historical", subtype = "harvest", stage = "smoothed",
                                      aggregate = FALSE, years = selectyears)[, , "mgrass.rainfed"]))
      mappingCountryCell  <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")

      if (calibrated == TRUE) {

        tau            <- calcOutput("LanduseIntensity", sectoral = "pasture", rescale = FALSE,
                                     aggregate = FALSE)[, selectyears, ]
        tauCell        <- toolAggregate(x = tau, rel = mappingCountryCell, from = "iso", to = "celliso", partrel = TRUE)

        yieldsPasture  <- tauCell * yieldsPasture
      }

      productionMAG    <- yieldsPasture * areaPasture

      #####################################################################
      # correct production mismatch - generic approach

      isoproductionMAG  <- isoMismatch <- toolAggregate(productionMAG, rel = mappingCountryCell,
                                                        from = "celliso", to = "iso")
      productionFAO     <- collapseNames(calcOutput("FAOmassbalance",
                                                    aggregate = FALSE)[, selectyears, "production"][, , "pasture"][, , "dm"])
      isoMismatch[]     <- abs(round(isoproductionMAG - toolIso2CellCountries(productionFAO), 4)) > 0

      if (any(isoMismatch != 0)) {

        # correct items with no area
        isoPastureArea   <- noPastureArea <- toolAggregate(areaPasture, rel = mappingCountryCell,
                                                           from = "celliso", to = "iso")
        noPastureArea[]  <- (isoPastureArea == 0) * isoMismatch

        if (any(noPastureArea != 0)) {

          # distribute equally over all cells
          productionMAG     <- productionMAG * (1 - noPastureArea) +
            magpie_expand(noPastureArea * toolIso2CellCountries(productionFAO), productionMAG) /
            new.magpie(names(getCPR(productionMAG)), fill = getCPR(productionMAG))

        }

        # correct items with no yields
        isoPastureYields   <- noPastureYields <- toolAggregate(yieldsPasture, weight = areaPasture,
                                                               rel = mappingCountryCell, from = "celliso", to = "iso")
        noPastureYields[]  <- (isoPastureYields == 0) * isoMismatch * (1 - noPastureArea)

        if (any(noPastureYields != 0)) {

          # distribute corresponding to pasture area share
          productionMAG     <- productionMAG * (1 - noPastureYields) +
            noPastureYields * toolAggregate(toolIso2CellCountries(productionFAO), rel = mappingCountryCell,
                                            weight = areaPasture, from = "iso", to = "celliso")
        }
      }

      isoproductionMAG  <- isoMismatch <- toolAggregate(productionMAG, rel = mappingCountryCell,
                                                        from = "celliso", to = "iso")
      isoMismatch[]     <- abs(round(isoproductionMAG - toolIso2CellCountries(productionFAO), 4)) > 0
      if (any(isoMismatch != 0)) warning("Cellular data to FAO production mismatch after generic fix. Please check.")
      #####################################################################

      prodAttributes <- calcOutput("Attributes", aggregate = FALSE)[, , "pasture"]
      productionMAG  <- collapseNames(productionMAG * prodAttributes)

    }

  } else if (products == "kli") {

    livestockTypes <- findset("kli")
    if (irrigation) {
      stop("Irrigation not yet implemented for this Product group")
    }
    if (!cellular) {

      productionMAG <- collapseNames(calcOutput("FAOmassbalance_pre", aggregate = FALSE)[, , livestockTypes][, , "production"])

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
