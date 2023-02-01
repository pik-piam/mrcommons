#' @title toolSmooth
#'
#' @description Smooth a time series using a given method and its default settings
#'
#' @param x magclass object that should be smoothed
#' @param method spline, average or more (See default argument for current default setting)
#'
#' @return smoothed data in magclass format
#' @author Kristine Karstens
#'
#' @importFrom madrat toolTimeSpline toolTimeAverage
#' @export

toolSmooth <- function(x, method = "spline") {

  if (!is.magpie(x)) stop("Input is not a MAgPIE object, x has to be a MAgPIE object!")

  if (method == "spline") {
    # current default is spline with 4 degrees of freedom per 100 years
    out <- toolTimeSpline(x, dof = 4)
  } else if (method == "average") {
    # backup and old default of 8-year averages
    out <- toolTimeAverage(x, averaging_range = 8, cut = FALSE)
  } else {
    stop("This method is not supported.")
  }

  return(out)
}
