#' @title calcVoPcrops
#' @description Calculates the value of production of individual production items or
#' its fraction compared to overall Value of Production (Agriculture,Fish,Forestry).
#'
#' @param fillGaps boolean: should gaps be filled using production * prices (where production data is available)?
#' @return magpie object. in mio. USD05 MER or fraction
#' @author Edna J. Molina Bacca, Debbora Leip
#' @importFrom dplyr intersect
#' @importFrom GDPuc convertGDP
#' @importFrom magpiesets findset
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("VoPcrops")
#' }
#'
calcVoPcrops <- function(fillGaps = TRUE) {

  # Value of production of individual items (current US$MER -> US$MER05)
  item <- "Gross_Production_Value_(current_thousand_US$)_(1000_US$)"
  vopAll <- readSource("FAO_online", "ValueOfProd")[, , item] / 1000 # mio. current US$MER

  getNames(vopAll) <- gsub("\\..*", "", getNames(vopAll))
  getNames(vopAll)[getNames(vopAll) == "254|Oil palm fruit"] <- "254|Oil, palm fruit"

  # items for aggregation
  mappingFAO <- toolGetMapping("FAO2LUH2MAG_croptypes.csv", type = "sectoral", where = "mrcommons")
  itemsIntersect <- intersect(getNames(vopAll), unique(mappingFAO$ProductionItem))
  mappingFAO <- mappingFAO[mappingFAO$ProductionItem %in% itemsIntersect, ]

  # Aggregation to magpie objects
  vopKcrAggregated <- toolAggregate(vopAll[, , itemsIntersect], rel = mappingFAO, from = "ProductionItem",
                                      to = "kcr", weight = NULL, dim = 3)

  # VoP in North Korea too high? -> excluded
  vopKcrAggregated["PRK", , ] <- 0

  # filling gaps based on production and prices (only works for 1991-2013, other years stay the same)
  if (isTRUE(fillGaps)) {
    kcr <- findset("kcr")
    vopKcrAggregated <- add_columns(vopKcrAggregated, setdiff(kcr, getNames(vopKcrAggregated)), dim = 3, fill = 0)

    production <- collapseDim(calcOutput("Production", products = "kcr", attributes = "dm", aggregate = FALSE))
    prices <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO", aggregate = FALSE))

    kPrices <- intersect(kcr, getNames(prices))
    prices <- prices[, , kPrices]

    # fill with region averages where possible
    pricesRegional <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO",
                                             aggregate = TRUE, regionmapping = "regionmappingH12.csv"))
    pricesRegional <- toolAggregate(pricesRegional, rel = toolGetMapping("regionmappingH12.csv"),
                                    from = "RegionCode", to = "CountryCode")[, , kPrices]
    prices[prices == 0] <- pricesRegional[prices == 0]

    # fill remaining gaps with global averages
    pricesGLO <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO", aggregate = "GLO"))[, , kPrices]
    pricesGLOfilledISO <- prices
    pricesGLOfilledISO[, , ] <- pricesGLO
    prices[prices == 0] <- pricesGLO[prices == 0]

    # add missing prices from calcIniFoodPrice for 2005 adjusted by applying average price development over time
    missingCommodities <- setdiff(kcr, getNames(prices))
    prices <- add_columns(prices, addnm = missingCommodities, dim = 3.1, fill = 0)[, , kcr]

    pricesGLOnormalized <- pricesGLO / setYears(pricesGLO[, 2005, ], NULL)
    weight <- dimSums(production, dim = 1)[, getYears(pricesGLOnormalized), getNames(pricesGLOnormalized)]
    averagePriceDevelopment <- dimSums(pricesGLOnormalized * (weight / dimSums(weight, dim = 3)), dim = 3)
    iniPrice <- calcOutput("IniFoodPrice", products = missingCommodities, aggregate = FALSE)
    iniPrice <- iniPrice * averagePriceDevelopment

    prices[, , missingCommodities] <- iniPrice[, , missingCommodities]

    # fill gaps in VoP (where production is available)
    years <- intersect(getYears(prices), getYears(production))
    calculatedVoP <- prices[, years, getNames(vopKcrAggregated)] * production[, years, getNames(vopKcrAggregated)]
    tmp <- vopKcrAggregated[, years, ]
    tmp[tmp == 0] <- calculatedVoP[tmp == 0]
    vopKcrAggregated[, years, ] <- tmp
  }

  vopKcrAggregated[!is.finite(vopKcrAggregated)] <- 0

  weight <- NULL
  units <- "USD05 MER"

 return(list(x = vopKcrAggregated,
            weight = weight,
            mixed_aggregation = NULL,
            unit = units,
            description = " Value of production for individual crops in 05USDMER"))
}
