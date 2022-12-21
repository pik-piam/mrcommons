#' @title calcMulticropping
#' @description calculates a multiple cropping factor based on area harvested,
#'              physical cropland area (and optionally fallow land).
#'
#' @param extend_future  if TRUE
#' @param factortype     CI: cropping intensity factor calculated as ratio of
#'                           harvested to physical area where values above one
#'                           indicate multicropping, below one fallow land (default)
#'                       MC: multiple cropping factor indicating areas that are
#'                           harvested more than once in one year calculated taking
#'                           fallow land into account explicitly:
#'                           harvestedArea / (physicalArea - fallowLand)
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, David Chen, Felicitas Beier
#' @seealso
#' [calcFAOLand()],
#' [calcCroparea()]
#' @examples
#' \dontrun{
#' calcOutput("Multicropping")
#' }
#'
calcMulticropping <- function(extend_future = FALSE, factortype = "CI") {   # nolint

  # physical cropland area ("6620|Cropland")
  phys   <- collapseNames(calcOutput("FAOLand", aggregate = FALSE)[, , "6620", pmatch = TRUE])
  # harvested area
  harv   <- collapseNames(dimSums(calcOutput("Croparea", physical = FALSE,
                                             aggregate = FALSE, sectoral = "kcr"),
                                  dim = 3.1))

  # match year dimension
  phys   <- phys[, intersect(getYears(phys), getYears(harv)), ]
  harv   <- harv[, intersect(getYears(phys), getYears(harv)), ]

  if (factortype == "CI") {

    # Cropping intensity (>1: mulitple cropping dominates; <1: fallow land dominates)
    out         <- harv / phys
    description <- "cropping intensity factor with values above one indicating multicropping, below one fallow land"

  } else if (factortype == "MC") {

    # fallow land ("6640|Land with temporary fallow")
    fallow <- collapseNames(calcOutput("FAOLand", aggregate = FALSE)[, , "6640", pmatch = TRUE])

    # match year dimension
    fallow <- fallow[, intersect(getYears(fallow), getYears(harv)), ]

    # Multiple cropping factor accounting for land that is left fallow
    out         <- harv / (phys - fallow)
    description <- "multiple cropping factor explicitly accounting for fallow land"

  } else {
    stop("Please select calculation method via the type argument in calcMulticropping")
  }

  out[is.na(out)] <- 0
  out[out == Inf] <- 0

  if (extend_future == TRUE) {
    out  <- toolHoldConstantBeyondEnd(out)
    phys <- toolHoldConstantBeyondEnd(phys)
  }

  return(list(x           = out,
              weight      = phys,
              unit        = "ratio",
              description = description))
}
