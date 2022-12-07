#' calcGDPppp
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is defunct. Use [mrdrivers::calcGDP()] instead.
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
  stop("calcGDPppp() is defunct. Please use mrdrivers::calcGDP() directly.")
}


#' calcGDPpppFuture
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' This function is defunct. Use [mrdrivers::calcGDPFuture()] instead.
#'
#' @param GDPpppFuture GDPppp future data source
#' @return GDP PPP(ICP11) in million USD05 equivalents
#' @seealso [mrdrivers::calcGDPFuture()]
calcGDPpppFuture <- function(GDPpppFuture = NULL) {
  stop("calcGDPpppFuture() is defunct. Please use mrdrivers::calcGDPFuture() directly.")
}

#' calcGDPpppPast
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' This function is defunct. Use [mrdrivers::calcGDPPast()] instead.
#'
#' @param GDPpppPast GDPppp future data source
#' @return GDP PPP(ICP11) in million USD05 equivalents
#' @seealso [mrdrivers::calcGDPPast()]
calcGDPpppPast <- function(GDPpppPast = "WDI-MI") {
  stop("calcGDPpppPast() is defunct. Please use mrdrivers::calcGDPPast() directly.")
}
