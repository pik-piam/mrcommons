#' @title calcGrowingPeriodMonths
#'
#' @description Calculates which gridcell-specific months in which
#'              growing conditions are favorable for crop growth
#'              based on monthly grass GPP
#'
#' @param selectyears    Years to be returned
#' @param lpjml          LPJmL version required for respective inputs: natveg or crop
#' @param climatetype    Switch between different climate scenarios or
#'                       historical baseline "GSWP3-W5E5:historical"
#' @param minThreshold   Threshold of monthly grass GPP to be classified as
#'                       growing period month
#'                       Unit of the threshold is gC/m^2.
#'                       Default: 100gC/m^2
#'                       Note: the default value is chosen based on LPJmL version 5
#'                             to reflect multiple cropping suitability as shown in GAEZ-4.
#'                             An update of LPJmL5 with regards to grass management may
#'                             require an adjustment of the threshold.
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("GrowingPeriodMonths", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass setYears getSets mbind getItems new.magpie
#'

calcGrowingPeriodMonths <- function(selectyears, lpjml, climatetype,
                                    minThreshold = 100) {
  ####################
  ### Definitions  ###
  ####################
  # Transformation factor for grass (gC/m^2 -> tDM/ha)
  yieldTransform <- 0.01 / 0.45

  ####################
  ### Read in data ###
  ####################
  # monthly grass GPP (in tDM/ha)
  grassGPPmonth <- setYears(calcOutput("GrassGPP", season = "monthly",
                                       lpjml = lpjml, climatetype = climatetype,
                                       selectyears = selectyears, aggregate = FALSE),
                            selectyears)

  ####################
  ### Calculations ###
  ####################
  # Calculate growing period
  grper       <- grassGPPmonth
  grper[, , ] <- 0
  # Classification as growing period month when monthly grass GPP > 100gC/m^2
  thresholdLGP <- minThreshold * yieldTransform
  grper[grassGPPmonth >= thresholdLGP] <- 1

  ##############
  ### Checks ###
  ##############
  if (any(is.na(grper))) {
    stop("mrland::calcGrowingPeriodMonths produced NA values")
  }
  if (any(grper < 0)) {
    stop("mrland::calcGrowingPeriodMonths produced negative values")
  }
  if (any(grper != 1 && grper != 0)) {
    stop("Problem in mrland::calcGrowingPeriodMonths: Value should be 0 or 1!")
  }

  ##############
  ### Return ###
  ##############
  unit        <- "boolean"
  description <- paste0("Classification of months as growing period month ",
                        "under irrigated and rainfed conditions. ",
                        "1 = suitable for crop growth, 0 = not suitable for crop growth")

  return(list(x            = grper,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
