#' @title calcFacReqGLO
#'
#' @description This function calculates 2005 global factor requirement costs (05USDMER/tDM) using FAO databases
#'
#' @return MAgPIE object
#' @author Edna J. Molina Bacca
#' @seealso [calcOutput()],[calcFactorIntensity()]
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

  .Deprecated(paste0("calcOutput", "(\"FacReq\", aggregate = \"GLO\", years = 2005)"))

  # Reads factor requirements in USD05ppp
  factors <- dimSums(calcOutput("FactorIntensity", aggregate = FALSE, years = 2005), dim = 3.1)

  # Reads production as weight
  weight <- collapseDim(calcOutput("Production", aggregate = FALSE, attributes = "dm", years = 2005))
  weight <- weight[, , getNames(factors)]
  weight[factors == 0] <- 0

  # Aggregated to global resolution
  x <- superAggregate(factors, aggr_type = "weighted_mean", level = "glo", weight = weight)

  # fill missing crops with maiz values
  kcr <- findset("kcr")
  missing <- c(where(!is.finite(x))$true$data, setdiff(kcr, getNames(x)))
  x <- add_columns(x, addnm = setdiff(kcr, getNames(x)), dim = 3.1)
  x[, , missing] <- x[, , "maiz"]

  return(list(x = x,
              weight = NULL,
              unit = "USD05MER per tDM",
              description = "Factor requirements for different crops (USD05MER per tDM) at global level"))
}
