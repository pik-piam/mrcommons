#' @title toolPatternScaling
#' @description This tool scales time series based on the approach used in the magpiemodel yield module.
#'
#' @param scen      time series of the scenario
#' @param scen_mean mean of scenario time series
#' @param ref_mean  mean of reference time series
#' @param ref_year  Reference year
#' @param variation 'yieldCalibMAG' (default); to be implemented: 'jensPaper'
#'
#' @return scaled data in magclass format
#' @author Kristine Karstens
#'
#' @importFrom magclass is.magpie
#' @importFrom madrat toolConditionalReplace
#'
#' @export

toolPatternScaling <- function(scen, scen_mean, ref_mean, ref_year = "y2010", variation = "yieldCalibMAG") {

  if (!is.magpie(scen) | !is.magpie(scen_mean) | !is.magpie(ref_mean)) {
    stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")
  }

  # check for negative range of values
  negative <- (any(scen < 0) | any(scen_mean < 0) | any(ref_mean < 0))

  # set years
  years       <- getYears(scen, as.integer = TRUE)
  after_ref   <- paste0("y", years[years >= as.numeric(substring(ref_year, 2))])

  # check if all objects contain ref year
  if (!(ref_year %in% Reduce(intersect, list(getYears(scen), getYears(scen_mean), getYears(ref_mean))))) {
    stop("Reference year is not included in all time series provided.")
  }

  # check if x and base are identical in dimension except time
  # TO-DO find a way of muliple checking

  # create new magpie object with full time horizon
  out       <- new.magpie(getCells(scen), after_ref, getNames(scen), sets = getSets(scen))

  scen      <- scen[, after_ref, ]
  scen_mean <- setYears(scen_mean[, ref_year, ], NULL)
  ref_mean  <- setYears(ref_mean[,  ref_year, ], NULL)

  ###########################################
  ### Use DELTA-approach to put signal of ###
  ### GCM data on historical observation  ###
  ### data from reference year +1 on      ###
  ###########################################

  lambda <- sqrt(scen_mean / ref_mean)
  lambda[scen_mean >= ref_mean] <- 1
  lambda[is.nan(lambda)]        <- 1

  out <- (1 + (ref_mean - scen_mean) / scen * toolConditionalReplace(scen / scen_mean,
                                                                     c("is.na()", "is.infinite()"), 1)**lambda)

  if (any((is.infinite(out) | is.na(out)) & scen != 0)) stop("Data containing inconsistencies.")
  out[is.na(out)]        <- 0
  out[is.infinite(out)]  <- 0
  out <- scen * out

  # check for nans and more
  if (any(is.infinite(out) | is.nan(out) | is.na(out))) warning("Data containing inconsistencies.")
  if (!negative & any(out < 0)) {
    vcat(2, paste0("toolPatternScaling created unwanted negativities in the range of ",
                   range(out[which(out < 0)]), ". They will be set to zero."))
    out[out < 0] <- 0
  }

  return(out)
}
