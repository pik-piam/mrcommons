#' @title calcFacReqGLO
#'
#' @description This function calculates 2005 global factor requirement costs (05USDMER/tDM) using FAO databases
#'
#' @return MAgPIE object
#' @author Edna J. Molina Bacca
#' @seealso \code{\link{calcOutput}},\code{\link{calcFactorIntensity}}
#' @importFrom luscale superAggregate
#' @importFrom magpiesets findset
#
#'
#' @examples
#' \dontrun{
#' calcOutput("calcFacReqGLO")
#' }
#'
calcFacReqGLO <- function() {

  # Reads factor requirements in USD05ppp
  Factors <- dimSums(calcOutput("FactorIntensity", aggregate = FALSE, years = 2005), dim = 3.1)

  # Reads production as weight
  weight <- collapseDim(calcOutput("Production", aggregate = FALSE, attributes = "dm", years = 2005))

  # Aggregated to global resolution
  x <- superAggregate(Factors, aggr_type = "weighted_mean", level = "glo", weight = weight[, , getNames(Factors)])

  # fills begr,betr and foddr with maiz values
  kcr <- findset("kcr")
  missing <- setdiff(kcr, getNames(x))
  x <- add_columns(x, addnm = missing, dim = 3.1)
  x[, , missing] <- x[, , "maiz"]




  return(list(x = x,
              weight = NULL,
              unit = "USD05MER per tDM",
              description = "Factor requirements for different crops (USD05MER per tDM) at global level"))
}
