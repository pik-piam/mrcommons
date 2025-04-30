#' Read Transport Costs
#'
#' Read in  Transport Costs from GTAP
#'
#'
#' @return Transport Costs in USD
#' @author David Chen
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource(type = "TransportCostsGTAP")
#' }
#'
readTransportCostsGTAP <- function() {
  costs <- read.csv("transport_costs.csv", header = FALSE)
  costs <- as.magpie(costs, spatial = NULL, temporal = NULL)
  return(costs)
}
