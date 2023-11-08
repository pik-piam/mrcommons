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
#' @importFrom withr local_options

calcRicearea <- function(cellular = FALSE, cells = "lpjcell", share = TRUE) {

  local_options(magclass_sizeLimit = 1e+12)

  selectyears <- findset("past")

  ############################################
  ### Ricearea and shares on country level ###
  ############################################

  # Country-level LUH flooded areas
  floodedLUHiso  <- collapseNames(calcOutput("LUH2v2", landuse_types = "flooded",
                                              cells = cells, aggregate = FALSE, irrigation = TRUE,
                                              cellular = FALSE, selectyears = "past"))

  # FAO rice areas (physical to be comparable with LUH)
  riceareaFAOiso <- collapseNames(calcOutput("Croparea", sectoral = "kcr", physical = TRUE,
                                              cellular = FALSE, cells = "magpicell", irrigation = FALSE,
                                              aggregate = FALSE)[, selectyears, "rice_pro"])

  # Country-level rice area
  ricearea <- floodedLUHiso

  # Correction for flooded non-rice areas (floodedLUHiso > riceareaFAOiso)
  ricearea[floodedLUHiso > riceareaFAOiso] <- riceareaFAOiso[floodedLUHiso > riceareaFAOiso]
  nonriceShr                               <- ifelse(floodedLUHiso > 0,
                                                       ricearea / floodedLUHiso,
                                                     0)

  # Correction for aerobic (non-paddy) rice (floodedLUHiso < riceareaFAOiso)
  floodedRicearea                                 <- riceareaFAOiso
  floodedRicearea[floodedLUHiso < riceareaFAOiso] <- floodedLUHiso[floodedLUHiso < riceareaFAOiso]
  floodedShr                                      <- ifelse(riceareaFAOiso > 0,
                                                              floodedRicearea / riceareaFAOiso,
                                                            0)

  # Non-flooded rice area
  nonfloodedRicearea <- ricearea * (1 - floodedShr)

  if (!cellular) {

    if (share) {

      out         <- 1 - floodedShr
      unit        <- "Share"
      description <- "Share of rice area that is non-flooded"

    } else {

      ricearea           <- add_dimension(ricearea, dim = 3.1,
                                          add = "type", nm = "total")
      floodedRicearea    <- add_dimension(floodedRicearea, dim = 3.1,
                                          add = "type", nm = "flooded")
      nonfloodedRicearea <- add_dimension(nonfloodedRicearea, dim = 3.1,
                                          add = "type", nm = "nonflooded")

      out         <- collapseNames(mbind(ricearea, floodedRicearea, nonfloodedRicearea))
      unit        <- "Mha"
      description <- "Physical rice area on country level"

    }

  } else {
    ############################################
    ### Ricearea and shares on cellular level
    ############################################

    # Cellular LUH flooded areas
    floodedLUH <- collapseNames(calcOutput("LUH2v2", landuse_types = "flooded",
                              cells = cells, cellular = TRUE, irrigation = TRUE,
                              selectyears = "past", aggregate = FALSE))

    # Correction for flooded non-rice areas (floodedLUHiso > riceareaFAOiso)
    if (cells == "magpiecell") {
      commonCountries <- intersect(getItems(nonriceShr, dim = "country"), getItems(floodedLUH, dim = "country"))
      ricearea   <- floodedLUH * toolIso2CellCountries(nonriceShr, cells = cells)
    } else if (cells == "lpjcell") {
      commonCountries <- intersect(getItems(nonriceShr, dim = "country"), getItems(floodedLUH, dim = "iso"))
      ricearea <- floodedLUH * nonriceShr[commonCountries, , ]
    } else {
      stop("When cellular==TRUE in calcRicearea: please select number of cells
            (magpiecell or lpjcell) via cells argument")
    }

    # Correction for aerobic (non-paddy) rice (floodedLUHiso < riceareaFAOiso)
    floodedRicearea    <- ricearea * floodedShr[commonCountries, , ]
    nonfloodedRicearea <- ricearea * (1 - floodedShr)[commonCountries, , ]
    ricearea           <- floodedRicearea + nonfloodedRicearea

    ricearea           <- add_dimension(ricearea, dim = 3.1, add = "type", nm = "total")
    floodedRicearea    <- add_dimension(floodedRicearea, dim = 3.1, add = "type", nm = "flooded")
    nonfloodedRicearea <- add_dimension(nonfloodedRicearea, dim = 3.1, add = "type", nm = "nonflooded")

    out         <- collapseNames(mbind(floodedRicearea, nonfloodedRicearea, ricearea))
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
