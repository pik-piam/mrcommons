#' @title calcVoP_crops
#' @description Calculates the value of production of individual production items or 
#' its fraction compared to overall Value of Production (Agriculture,Fish,Forestry).
#'
#'
#'
#'
#' @param output defines if output should be given as an "absolute" value or 
#' as a "fraction" of the overall value of production.
#' @param units if output units should be in mio. "USD05", mio. of "USD15", "current" or NULL for output "fraction" type
#' @return magpie object. in current mio. USD units, in mio. USD05, current USD or NULL (fraction).
#' @author Edna J. Molina Bacca
#' @importFrom dplyr mutate
#' @importFrom luscale speed_aggregate
#' @importFrom dplyr intersect
#'
#' @seealso \code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' a <- calcOutput("VoP_crops")
#' }
#'
calcVoP_crops <- function(output = "absolute", units = "USD05") {

  #### GDP
  GDP <- calcOutput("GDPppp", aggregate = FALSE)[, c(2020, 2015, 2005), "gdp_SSP2"]
  GDP_con <- setNames(setYears((GDP[, 2020, ] / GDP[, 2015, ]), NULL), NULL)

  # Value of production for Agriculture, forestry and fishes
  VoP_AFF <- calcOutput("VoP_AFF", aggregate = FALSE)
  VoP_Total <- dimSums(VoP_AFF, dim = 3) # mio. current USD

  # Value of production of indiviual items
  VoP_All <- readSource("FAO_online", "ValueOfProd")[, ,"Gross_Production_Value_(constant_2014_2016_thousand_I$)_(1000_Int_$)"] / 1000 * GDP_con
  getNames(VoP_All) <- gsub("\\..*", "", getNames(VoP_All))
  getNames(VoP_All)[getNames(VoP_All) == "257|Oil, palm"] <- "257|Oil palm"

  # items for aggregation
  mappingFAO <- toolGetMapping("FAO_VoP_kcr.csv", type = "sectoral")
  items_intersect <- intersect(getNames(VoP_All), unique(mappingFAO$ProductionItem))
  mappingFAO <- mappingFAO[mappingFAO$ProductionItem %in% items_intersect, ]

  # Aggregation to magpie objects
  VoP_kcr_aggregated <- toolAggregate(VoP_All[, , items_intersect], rel = mappingFAO, from = "ProductionItem",
                                      to = "k", weight = NULL, dim = 3)

  years <- intersect(getYears(VoP_kcr_aggregated), getYears(VoP_Total))

  # if desired output is fraction over overall value of production (Agriculture, forestry, fishery) or absolute value
  x <- if (output == "fraction") VoP_kcr_aggregated[, years, ] / VoP_Total[, years, ] else VoP_kcr_aggregated

  x[!is.finite(x)] <- 0


  if (units == "USD05" & output == "absolute") {
    GDP_con <- setNames(setYears((GDP[, 2005, ] / GDP[, 2020, ]), NULL), NULL)
  } else if (units == "USD15" & output == "absolute") {
    GDP_con <- setNames(setYears((GDP[, 2015, ] / GDP[, 2020, ]), NULL), NULL)
  } else if (units == "current" | units == NULL) {
    GDP_con <- 1
  } else {
       stop("Not a valid unit")
     }

  x <- x * GDP_con

  if (output == "absolute") {
    weight <- NULL
  } else if (output == "fraction") {
    Production <- collapseNames(calcOutput("Production", aggregate = FALSE, products = "kcr", attributes = "dm"))
    years <- intersect(getYears(Production), getYears(x))
    names <- intersect(getNames(Production), getNames(x))

    weight <- Production[, years, names]
    x <- x[, years, names]
    weight[x == 0] <- 0

  } else {
    stop("Output not supported")
  }


 return(list(x = x,
                weight = weight,
                mixed_aggregation = NULL,
                unit = units,
                description = " Value of production for individual crops"))
}
