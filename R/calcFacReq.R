#' @title calcFacReq
#'
#' @description This function calculates factor requirement costs (05USDMER/tDM) on regional level,
#' based FAO databases
#'
#' @param splitSectors if TRUE requirements for labor and capital will be reported separately
#' @return MAgPIE object
#' @author Debbora Leip
#' @seealso [calcOutput()],[calcFactorIntensity()]
#' @importFrom luscale superAggregate
#' @importFrom magpiesets findset
#
#'
#' @examples
#' \dontrun{
#' calcOutput("calcFacReq")
#' }
#'
calcFacReq <- function(splitSectors = FALSE) {

  # get factor requirements in US$PPP05
  facReq <- calcOutput("FactorIntensity", aggregate = FALSE)
  facReq <- facReq[, where(facReq != 0)$true$years, ]

  # fill missing crops with maize values
  kcr <- findset("kcr")
  missing <- c(where(dimSums(facReq, c(1, 3.1)) == 0)$true$data, setdiff(kcr, getNames(facReq, dim = 2)))
  facReq <- add_columns(facReq, addnm = setdiff(kcr, getNames(facReq, dim = 2)), dim = 3.2)
  facReq[, , missing] <- facReq[, , "maiz"]

  # production as aggregation weight (using maize production as weight for missing crops)
  years <- getItems(facReq, dim = 2)
  weight1 <- collapseDim(calcOutput("Production", aggregate = FALSE, attributes = "dm", years = years))
  weight1[, , missing] <- weight1[, , "maiz"]
  weight1 <- weight1[, , getNames(facReq, dim = 2)]
  weight1[dimSums(facReq, dim = 3.1) == 0] <- 0

  # fill missing values with global averages
  weight <- gloAvg <- facReq
  weight[, , ] <- weight1
  gloAvg[, , ] <- superAggregate(facReq, aggr_type = "weighted_mean", level = "glo", weight = weight)
  facReq[facReq == 0] <- gloAvg[facReq == 0]

  # set weights for values filled with global average to very low value (-> only affect aggregation in regions with no
  # original values)
  weight[weight == 0] <- 1e-20

  # combine labor and capital requirements
  if (!isTRUE(splitSectors)) {
    facReq <- dimSums(facReq, dim = 3.1)
    weight <- collapseDim(weight[, , "Labor"], dim = 3.1)
  }

  return(list(x = facReq,
              unit = "USD05MER per tDM",
              weight = weight,
              description = "Factor requirements for different crops (USD05MER per tDM) at regional level",
              isocountries = FALSE))
}
