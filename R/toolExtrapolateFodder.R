#' @title       toolExtrapolateFodder
#' @description Extrapolate fodder data, based on two time steps (5-averages around this years)
#' @param x input data
#' @param exyears two years
#' @param average the averaging_range in toolTimeInterpolate
#' @param endyear year till when it should be extrapolated
#' @return magpie object including extrapolated years
#' @author Kristine Karstens
#'
#' @export

toolExtrapolateFodder <- function(x, exyears = c(2004, 2009), average = 5, endyear = 2015) {

  if (endyear <= max(getYears(x, as.integer = TRUE))) return(x)

  dt  <- floor(average / 2)
  tmp <- time_interpolate(mbind(toolTimeAverage(x[, seq(exyears[1] - dt, exyears[1] + dt), ], average),
                                 toolTimeAverage(x[, seq(exyears[2] - dt, exyears[2] + dt), ], average)),
                           c(2012:endyear), extrapolation_type = "linear")

  tmp <- toolConditionalReplace(tmp, "<0", 0)
  x   <- mbind(x, tmp)

  return(x)
}
