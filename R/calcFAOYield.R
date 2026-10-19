#' @title calcFAOYield
#'
#' @description calculates the yield based on FAO data
#' @param physical   physical area or havested area
#' @param attributes in dm, wm, ge, nr, p, k
#' @param cellular   if TRUE value is calculate on cellular level
#' @param areaSource data source for croparea used in calculation: FAO or Toolbox
#' @param irrigation distinguish irrigation or not
#' @param cut        FALSE (default) - do not cut off yields,
#'                   number between 0 and 1 to define percentile value for cut off
#' @param average    averaging period in years (if NULL no averaging is used)
#' @return MAgPIE object of yields
#' @author Debbora Leip, Jan Philipp Dietrich, Kristine Karstens, Felicitas Beier
#' @importFrom stats quantile

calcFAOYield <- function(physical = TRUE, attributes = "dm", irrigation = FALSE,
                         cellular = FALSE, cut = FALSE, average = 5, areaSource = "FAO") {

  production <- calcOutput("Production", products = "kcr", attributes = attributes,
                           irrigation = irrigation, cellular = cellular,
                           aggregate = FALSE)
  selectyears <- getItems(production, dim = "year")

  if (areaSource == "FAO") {

    area <- calcOutput("Croparea", sectoral = "kcr", physical = physical,
                       cellular = cellular,
                       irrigation = irrigation, aggregate = FALSE)

  } else if (areaSource == "LandInG") {

    area <- calcOutput("CropareaLandInG", sectoral = "kcr", physical = physical,
                       irrigation = irrigation, selectyears = selectyears,
                       cellular = cellular, aggregate = FALSE)
  } else {
    stop("Please specify which area should be used for calculation.
         Note: LandInG should be FAO-consistent.")
  }

  faoyears   <- intersect(getYears(production), getYears(area))

  yield      <- ifelse(area[, faoyears, ] > 1e-6,
                        collapseNames(production[, faoyears, ]) / area[, faoyears, ],
                       NA)
  yield[yield == Inf | yield == -Inf | is.nan(yield) | yield == 0] <- NA

  # If cut!=FALSE, cut yields at 'cut'-percentile and hold constant from there on
  if (cut != FALSE) {
    for (k in getNames(yield)) {
      # define cut off, depending on 'cut' value ('cut'-percentile)
      cutK <- quantile(yield[, , k], cut, na.rm = TRUE)
      # set all values above the threshold to cut off value
      yield[, , k][yield[, , k] > cutK] <- cutK
    }
  }

  # if no data for begr and betr is available just take highest observed yields for the given country as replacement
  max <- as.magpie(suppressWarnings(apply(yield, 1:2, max, na.rm = TRUE)))
  max[max == -Inf] <- NA
  for (b in c("begr", "betr")) {
    if (all(is.na(yield[, , b]))) {
      yield[, , b] <- max
    }
  }

  # use lower end yield values as replacements for missing data points
  tmp       <- as.magpie(apply(yield, 2:3, quantile, probs = 0.2, na.rm = TRUE))
  low       <- yield
  low[, , ] <- tmp
  na        <- which(is.na(yield), arr.ind = TRUE)
  yield[na] <- low[na]

  if (!is.null(average)) {
    area    <- madrat::toolTimeAverage(area, average)
    yield   <- madrat::toolTimeAverage(yield, average)
  }

  cyears <- intersect(getYears(yield), getYears(area))
  yield <- yield[, cyears, ]
  area <- area[, cyears, ]

  return(list(x            = yield,
              weight       = area + 10^-10,
              min          = 0,
              unit         = "t/ha",
              description  = "Calculates the yield based on FAO",
              isocountries = !cellular))
}
