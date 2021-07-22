#' @title calcRicearea
#' @description calculates rice area based on LUH flooded areas and
#'              physical rice areas reported by FAOSTAT.
#'
#' @param cellular   If TRUE: calculates cellular rice area
#' @param cells      Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param share      If TRUE: non-flooded share is returned.
#'                   If FALSE: rice area (flooded, non-flooded, total) in Mha is returned
#'
#' @return rice areas or rice area shares of flooded and non-flooded category
#'
#' @author Felicitas Beier, Kristine Karstens
#'
#' @importFrom magpiesets findset

calcRicearea <- function(cellular = FALSE, cells = "magpiecell", share = TRUE) {

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit = 1e+12)
  on.exit(options(magclass_sizeLimit = sizelimit))

  selectyears <- findset("past")

  ############################################
  ### Ricearea and shares on country level ###
  ############################################

  # Country-level LUH flooded areas
  LUH_flooded_iso  <- collapseNames(calcOutput("LUH2v2", landuse_types = "flooded",
                                               cells = cells, aggregate = FALSE, irrigation = TRUE,
                                               cellular = FALSE, selectyears = "past"))

  # FAO rice areas (physical to be comparable with LUH)
  FAO_ricearea_iso <- collapseNames(calcOutput("Croparea", sectoral = "kcr", physical = TRUE,
                                               cellular = FALSE, cells = "magpicell", irrigation = FALSE,
                                               aggregate = FALSE)[, selectyears, "rice_pro"])

  # Country-level rice area
  ricearea <- LUH_flooded_iso

  # Correction for flooded non-rice areas (LUH_flooded_iso > FAO_ricearea_iso)
  ricearea[LUH_flooded_iso > FAO_ricearea_iso] <- FAO_ricearea_iso[LUH_flooded_iso > FAO_ricearea_iso]
  nonriceShr                                   <- ifelse(LUH_flooded_iso > 0, ricearea / LUH_flooded_iso, 0)

  # Correction for aerobic (non-paddy) rice (LUH_flooded_iso < FAO_ricearea_iso)
  ricearea_flooded                                     <- FAO_ricearea_iso
  ricearea_flooded[LUH_flooded_iso < FAO_ricearea_iso] <- LUH_flooded_iso[LUH_flooded_iso < FAO_ricearea_iso]
  floodedShr                                           <- ifelse(FAO_ricearea_iso > 0,
                                                                 ricearea_flooded / FAO_ricearea_iso, 0)

  # Non-flooded rice area
  ricearea_nonflooded <- ricearea * (1 - floodedShr)

  if (!cellular) {

    if (share) {

      out         <- floodedShr
      unit        <- "Share"
      description <- "Share of rice area that is flooded"

    } else {

      ricearea            <- add_dimension(ricearea, dim = 3.1, add = "type", nm = "total")
      ricearea_flooded    <- add_dimension(ricearea_flooded, dim = 3.1, add = "type", nm = "flooded")
      ricearea_nonflooded <- add_dimension(ricearea_nonflooded, dim = 3.1, add = "type", nm = "nonflooded")

      out         <- collapseNames(mbind(ricearea, ricearea_flooded, ricearea_nonflooded))
      unit        <- "Mha"
      description <- "Physical rice area on country level"

    }

  } else {

    ############################################
    ### Ricearea and shares on cellular level
    ############################################

    # Cellular LUH flooded areas
    LUH_flooded <- collapseNames(calcOutput("LUH2v2", landuse_types = "flooded",
                              cells = cells, cellular = TRUE, irrigation = TRUE,
                              selectyears = "past", aggregate = FALSE))

    # Correction for flooded non-rice areas (LUH_flooded_iso > FAO_ricearea_iso)
    ricearea <- LUH_flooded * toolIso2CellCountries(nonriceShr, cells = cells)

    # Correction for aerobic (non-paddy) rice (LUH_flooded_iso < FAO_ricearea_iso)
    floodedShr          <- toolIso2CellCountries(floodedShr, cells = cells)
    ricearea_flooded    <- ricearea * floodedShr
    ricearea_nonflooded <- ricearea * (1 - floodedShr)
    ricearea            <- ricearea_flooded + ricearea_nonflooded

    ricearea            <- add_dimension(ricearea, dim = 3.1, add = "type", nm = "total")
    ricearea_flooded    <- add_dimension(ricearea_flooded, dim = 3.1, add = "type", nm = "flooded")
    ricearea_nonflooded <- add_dimension(ricearea_nonflooded, dim = 3.1, add = "type", nm = "nonflooded")

    out         <- collapseNames(mbind(ricearea_flooded, ricearea_nonflooded, ricearea))
    unit        <- "Mha"
    description <- "Physical rice area on cellular level"

    if (share) {
      stop("Argument share = TRUE not supported with cellular = TRUE.
           Please select cellular = FALSE to return flooded rice area share")
    }

  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = !cellular))
}
