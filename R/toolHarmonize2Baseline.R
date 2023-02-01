#' toolHarmonize2Baseline
#'
#' @param x magclass object that should be set on baseline
#' @param base magclass object for baseline
#' @param ref_year Reference year
#' @param method additive: x is harmonized to base by additive factor
#'               multiplicative: x is harmonized to base by multiplicative factor
#'               limited: multiplicative harmonization,
#'               but for an underestimated baseline the signal is
#'               limited to the additive term rather than the multiplicative factor
#' @param hard_cut Switch to TRUE for data that can not be harmonized, but have to be glued together
#'
#' @return the averaged data in magclass format
#' @author Kristine Karstens, Felicitas Beier
#'
#' @export

toolHarmonize2Baseline <- function(x,
                                   base,
                                   ref_year = "y2015", # nolint: object_name_linter
                                   method = "limited",
                                   hard_cut = FALSE # nolint: object_name_linter
) {
  if (!is.magpie(x) || !is.magpie(base)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")

  # check for negative range of values
  negative <- (any(x < 0) | any(base < 0))

  # check if years are overlapping and refs is part of both time horizons
  if (!ref_year %in% intersect(getYears(x), getYears(base))) {
    stop("Overlapping time period of baseline and data is not including the reference year!")
  }

  # set years
  years <- sort(union(getYears(base), getYears(x)))
  tillRef <- getYears(base, as.integer = TRUE)
  tillRef <- paste0("y", tillRef[tillRef <= as.numeric(substring(ref_year, 2))])
  afterRef <- getYears(x, as.integer = TRUE)
  afterRef <- paste0("y", afterRef[afterRef > as.numeric(substring(ref_year, 2))])


  # check if x and base are identical in dimension except time
  if (!setequal(getCells(x), getCells(base)) || !setequal(getNames(x), getNames(base))) {
    stop("Dimensions of the MAgPIE objects do not match!")
  }

  # create new magpie object with full time horizon
  full <- new.magpie(getCells(x), years, getNames(x), sets = getSets(x))

  full <- as.array(full)
  x <- as.array(x)
  base <- as.array(base)

  # from start until ref_year, use the corresponding ref value
  full[, tillRef, ] <- base[, tillRef, ]

  repRefYear <- rep(ref_year, length(afterRef))

  if (hard_cut) {
    ###########################################
    ### Use GCM data after historical data  ###
    ### from reference year +1 on           ###
    ###########################################

    full[, afterRef, ] <- x[, afterRef, ]
  } else if (method == "multiplicative") {
    full[, afterRef, ] <- x[, afterRef, ] * (base[, repRefYear, ] / x[, repRefYear, ])

    # correct NAs and infinite
    fullNotFinite <- !is.finite(full[, afterRef, ])
    # does this make sense?
    full[, afterRef, ][fullNotFinite] <- (base[, repRefYear, ] + x[, afterRef, ])[fullNotFinite]
  } else if (method == "additive") {
    full[, afterRef, ] <- x[, afterRef, ] + (base[, repRefYear, ] - x[, repRefYear, ])
  } else if (method == "limited") {
    ###########################################
    ### Use DELTA-approach to put signal of ###
    ### GCM data on historical observation  ###
    ### data from reference year +1 on      ###
    ###########################################

    lambda <- sqrt(x[, ref_year, , drop = FALSE] / base[, ref_year, , drop = FALSE])
    lambda[base[, ref_year, ] <= x[, ref_year, ]] <- 1
    lambda[is.nan(lambda)] <- 1
    lambda <- lambda[, repRefYear, ]

    full[, afterRef, ] <-
      base[, repRefYear, ] +
      (x[, afterRef, ] - x[, repRefYear, ]) * (base[, repRefYear, ] / x[, repRefYear, ])**lambda

    full[, afterRef, ][is.na(full[, afterRef, ])] <- 0
  } else {
    stop("Please select harmonization method (additive, multiplicative, limited (default))")
  }

  # check for nans and more
  if (any(is.infinite(full) | is.nan(full) | is.na(full))) {
    warning("Data containing inconsistencies.")
  }
  if (!negative && any(full < 0)) {
    vcat(2, paste0(
      "toolHarmonize2Baseline created unwanted negativities in the range of ",
      range(full[which(full < 0)]),
      ". They will be set to zero."
    ))
    full[full < 0] <- 0
  }

  out <- as.magpie(full, spatial = 1)

  return(out)
}
