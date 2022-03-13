#' @title calcVoP_crops
#' @description Calculates the value of production of individual production items or
#' its fraction compared to overall Value of Production (Agriculture,Fish,Forestry).
#'
#' @param output defines if output should be given as an "absolute" value or
#' as a "fraction" of the overall value of production.
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
#' a <- calcOutput("VoP_crops")
#' }
#'
calcVoP_crops <- function(output = "absolute", fillGaps = TRUE) {

  # Value of production for Agriculture, forestry and fishes
  VoP_AFF <- calcOutput("VoP_AFF", aggregate = FALSE)
  VoP_Total <- dimSums(VoP_AFF, dim = 3) # mio. 05USD MER

  # Value of production of individual items (current US$MER -> US$MER05)
  item <- "Gross_Production_Value_(current_thousand_US$)_(1000_US$)"
  VoP_All_currentMER <- readSource("FAO_online", "ValueOfProd")[, , item] / 1000 # mio. current US$MER
  VoP_All <- suppressWarnings(convertGDP(VoP_All_currentMER,
                                         unit_in = "current US$MER",
                                         unit_out = "constant 2005 US$MER"))
  # for countries with missing inflation factors we assume no inflation:
  VoP_All[is.na(VoP_All)] <- VoP_All_currentMER[is.na(VoP_All)]

  getNames(VoP_All) <- gsub("\\..*", "", getNames(VoP_All))
  getNames(VoP_All)[getNames(VoP_All) == "254|Oil palm fruit"] <- "254|Oil, palm fruit"

  # items for aggregation
  mappingFAO <- toolGetMapping("FAO2LUH2MAG_croptypes.csv", type = "sectoral", where = "mrcommons")
  itemsIntersect <- intersect(getNames(VoP_All), unique(mappingFAO$ProductionItem))
  mappingFAO <- mappingFAO[mappingFAO$ProductionItem %in% itemsIntersect, ]

  # Aggregation to magpie objects
  VoP_kcr_aggregated <- toolAggregate(VoP_All[, , itemsIntersect], rel = mappingFAO, from = "ProductionItem",
                                      to = "kcr", weight = NULL, dim = 3)

  # VoP in North Korea too high? -> excluded
  VoP_kcr_aggregated["PRK", , ] <- 0

  # filling gaps based on production and prices
  if (isTRUE(fillGaps)) {
    kcr <- findset("kcr")
    VoP_kcr_aggregated <- add_columns(VoP_kcr_aggregated, setdiff(kcr, getNames(VoP_kcr_aggregated)), dim = 3, fill = 0)

    production <- collapseDim(calcOutput("Production", products = "kcr", attributes = "dm", aggregate = FALSE))
    prices <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO", aggregate = FALSE))

    kPrices <- intersect(kcr, getNames(prices))
    prices <- prices[, , kPrices]

    # fill with region averages where possible
    pricesRegional <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO", aggregate = TRUE))
    pricesRegional <- toolAggregate(pricesRegional, rel = toolGetMapping(getConfig()$regionmapping),
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
    averagePriceDevelopment <- dimSums(pricesGLOnormalized, dim = 3) / length(kPrices)
    iniPrice <- calcOutput("IniFoodPrice", products = missingCommodities, aggregate = FALSE)
    iniPrice <- iniPrice * averagePriceDevelopment

    prices[, , missingCommodities] <- iniPrice[, , missingCommodities]

    # fill gaps in VoP (where production is available)
    years <- intersect(getYears(prices), getYears(production))
    calculatedVoP <- prices[, years, getNames(VoP_kcr_aggregated)] * production[, years, getNames(VoP_kcr_aggregated)]
    VoP_kcr_aggregated <- VoP_kcr_aggregated[, years, ]
    VoP_kcr_aggregated[VoP_kcr_aggregated == 0] <- calculatedVoP[VoP_kcr_aggregated == 0]
  }

  years <- intersect(getYears(VoP_kcr_aggregated), getYears(VoP_Total))

  # if desired output is fraction over overall value of production (Agriculture, forestry, fishery) or absolute value
  x <- if (output == "fraction") VoP_kcr_aggregated[, years, ] / VoP_Total[, years, ] else VoP_kcr_aggregated

  x[!is.finite(x)] <- 0

  if (output == "absolute") {
    weight <- NULL
    units <- "USD05 MER"
  } else if (output == "fraction") {
    production <- collapseNames(calcOutput("Production", aggregate = FALSE, products = "kcr", attributes = "dm"))
    years <- intersect(getYears(production), getYears(x))
    names <- intersect(getNames(production), getNames(x))

    weight <- production[, years, names]
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
