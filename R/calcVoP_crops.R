#' @title calcVoP_crops
#' @description Calculates the value of production of individual production items or
#' its fraction compared to overall Value of Production (Agriculture,Fish,Forestry).
#'
#'
#'
#'
#' @param output defines if output should be given as an "absolute" value or
#' as a "fraction" of the overall value of production.
#' @return magpie object. in mio. USD05 MER or fraction
#' @author Edna J. Molina Bacca
#' @importFrom dplyr mutate
#' @importFrom luscale speed_aggregate
#' @importFrom dplyr intersect
#' @importFrom GDPuc convertGDP
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("VoP_crops")
#' }
#'
calcVoP_crops <- function(output = "absolute") {

  # Value of production for Agriculture, forestry and fishes
  VoP_AFF <- calcOutput("VoP_AFF", aggregate = FALSE)
  VoP_Total <- dimSums(VoP_AFF, dim = 3) # mio. 05USD MER

  # Value of production of individual items (US$PPP2015 -> US$MER05)
  item <- "Gross_Production_Value_(constant_2014_2016_thousand_I$)_(1000_Int_$)"
  VoP_All_ppp <- readSource("FAO_online", "ValueOfProd")[, , item] / 1000
  VoP_All <- convertGDP(VoP_All_ppp,
                        unit_in = "constant 2015 Int$PPP",
                        unit_out = "constant 2005 US$MER")
  # for countries with missing conversion factor we assume PPP = MER and no inflation:
  VoP_All[is.na(VoP_All)] <- VoP_All_ppp[is.na(VoP_All)]
  getNames(VoP_All) <- gsub("\\..*", "", getNames(VoP_All))
  getNames(VoP_All)[getNames(VoP_All) == "254|Oil palm fruit"] <- "254|Oil, palm fruit"

  # items for aggregation
  mappingFAO <- toolGetMapping("FAO2LUH2MAG_croptypes.csv", type = "sectoral", where = "mrcommons")
  items_intersect <- intersect(getNames(VoP_All), unique(mappingFAO$ProductionItem))
  mappingFAO <- mappingFAO[mappingFAO$ProductionItem %in% items_intersect, ]

  # Aggregation to magpie objects
  VoP_kcr_aggregated <- toolAggregate(VoP_All[, , items_intersect], rel = mappingFAO, from = "ProductionItem",
                                      to = "kcr", weight = NULL, dim = 3)

  years <- intersect(getYears(VoP_kcr_aggregated), getYears(VoP_Total))

  # if desired output is fraction over overall value of production (Agriculture, forestry, fishery) or absolute value
  x <- if (output == "fraction") VoP_kcr_aggregated[, years, ] / VoP_Total[, years, ] else VoP_kcr_aggregated

  x[!is.finite(x)] <- 0


  if (output == "absolute") {
    weight <- NULL
    units <- "USD05 MER"
  } else if (output == "fraction") {
    Production <- collapseNames(calcOutput("Production", aggregate = FALSE, products = "kcr", attributes = "dm"))
    years <- intersect(getYears(Production), getYears(x))
    names <- intersect(getNames(Production), getNames(x))

    weight <- Production[, years, names]
    x <- x[, years, names]
    weight[x == 0] <- 0
    units <- "fraction"

  } else {
    stop("Output not supported")
  }



 return(list(x = x,
            weight = weight,
            mixed_aggregation = NULL,
            unit = units,
            description = " Value of production for individual crops in 05USDMER"))
}
