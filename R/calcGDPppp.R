#' calcGDPppp
#' 
#' @description 
#'  `r lifecycle::badge('deprecated')`
#' 
#' @param GDPpppCalib to what should be calibrated? past, future or a transition?
#' @param GDPpppPast GDPppp past data source
#' @param GDPpppFuture GDPppp future data source
#' @param FiveYearSteps Only five year steps if TRUE, FALSE returns years from source data
#' @param naming naming scheme
#' 
#' @return GDP PPP(ICP11) in million USD05 equivalents
#' @seealso [mrdrivers::calcGDP()]
#' 
calcGDPppp <- function(GDPpppCalib = NULL,
                       GDPpppPast = NULL, 
                       GDPpppFuture = NULL,
                       FiveYearSteps = TRUE,
                       naming = NULL) {
  warning("calcGDPppp() is deprecated. Returning default 'calcOutput(GDP)' output instead. Please use mrdrivers::calcGDP() directly.")
  x <- calcOutput("GDP", FiveYearSteps = FiveYearSteps, aggregate = FALSE, supplementary = TRUE)
  list(x = x$x,
       weight = x$weight,
       unit = x$unit,
       description = x$description)
}


#' calcGDPpppFuture
#' 
#' @description 
#' `r lifecycle::badge('deprecated')`
#'
#' @param GDPpppFuture GDPppp future data source
#' @return GDP PPP(ICP11) in million USD05 equivalents
#' @seealso [mrdrivers::calcGDPFuture()]
calcGDPpppFuture <- function(GDPpppFuture = NULL) {
  warning("calcGDPpppFuture() is deprecated. Please use mrdrivers::calcGDPFuture() directly.")
  x <- calcOutput("GDPFuture", aggregate = FALSE, supplementary = TRUE)
  list(x = x$x,
       weight = x$weight,
       unit = x$unit,
       description = x$description)
}

#' calcGDPpppPast
#' 
#' @description 
#' `r lifecycle::badge('deprecated')`
#'
#' @param GDPpppPast GDPppp future data source
#' @return GDP PPP(ICP11) in million USD05 equivalents
#' @seealso [mrdrivers::calcGDPPast()]
calcGDPpppPast <- function(GDPpppPast = "WDI-MI") {
  warning("calcGDPpppPast() is deprecated. Please use mrdrivers::calcGDPPast() directly.")
  x <- calcOutput("GDPPast", GDPPast = GDPpppPast, aggregate = FALSE, supplementary = TRUE)
  list(x = x$x,
       weight = x$weight,
       unit = x$unit,
       description = x$description)
}