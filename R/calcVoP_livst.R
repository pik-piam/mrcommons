#' @title calcVoP_livst
#' @description Calculates the value of production of individual livestock categories
#'
#' @param other boolean: should FAO livestock categories that can't be matched to MAgPIE categories (i.e. beeswax, wool,
#' silkworms, and honey) be reported as "livst_other"?
#' @param fillGaps boolean: should gaps be filled using production * prices (where production data is available)?
#' @return magpie object. in mio. USDMER05
#' @author Debbora Leip
#' @importFrom GDPuc convertGDP
#' @importFrom magpiesets findset
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("VoP_livst")
#' }
#'
calcVoP_livst <- function(other = FALSE, fillGaps = FALSE) {

  # Value of production of individual items (current US$MER -> US$MER05)
  item <- "Gross_Production_Value_(current_thousand_US$)_(1000_US$)"
  VoPcurrentMER <- readSource("FAO_online", "ValueOfProd")[, , item] / 1000 # mio. current US$MER
  VoP <- suppressWarnings(convertGDP(VoPcurrentMER,
                                     unit_in = "current US$MER",
                                     unit_out = "constant 2005 US$MER"))
  # for countries with missing inflation factors we assume no inflation:
  VoP[is.na(VoP)] <- VoPcurrentMER[is.na(VoP)]

  # mapping for aggregation
  mappingFAO <- toolGetMapping("FAO_VoP_kli.csv", type = "sectoral", where = "mrcommons")
  if (isFALSE(other)) {
    mappingFAO <- mappingFAO[mappingFAO$kli != "livst_other", ]
  }
  items <- mappingFAO$FAO_item

  # aggregation to magpie objects (and livst_other)
  VoPlivst <- toolAggregate(VoP[, , items], rel = mappingFAO, from = "FAO_item", to = "kli", weight = NULL, dim = 3)

  # filling gaps based on production and prices
  if (isTRUE(fillGaps)) {
    kli <- findset("kli")
    production <- collapseDim(calcOutput("Production", products = "kli", attributes = "dm", aggregate = FALSE))
    prices <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO", aggregate = FALSE))

    # fill with region averages where possible
    pricesRegional <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO", aggregate = TRUE))
    pricesRegional <- toolAggregate(pricesRegional, rel = toolGetMapping(getConfig()$regionmapping),
                                    from = "RegionCode", to = "CountryCode")
    prices[prices == 0] <- pricesRegional[prices == 0]

    # fill remaining gaps with global averages
    pricesGLO <- prices
    pricesGLO[, , ] <- collapseDim(calcOutput(type = "PriceAgriculture", datasource = "FAO", aggregate = "GLO"))
    prices[prices == 0] <- pricesGLO[prices == 0]

    prices <- prices[, , kli]

    # fill gaps in VoP (where production is available)
    years <- intersect(getYears(prices), getYears(production))
    VoPlivst <- VoPlivst[, years, ]
    if (isTRUE(other)) {
      VoPother <- VoPlivst[, , "livst_other"]
      VoPlivst <- VoPlivst[, , "livst_other", invert = TRUE]
    }
    calculatedVoP <- prices[, years, getNames(VoPlivst)] * production[, years, getNames(VoPlivst)]
    VoPlivst[VoPlivst == 0] <- calculatedVoP[VoPlivst == 0]
    if (isTRUE(other)) VoPlivst <- mbind(VoPlivst, VoPother)
  }

  return(list(x = VoPlivst,
              weight = NULL,
              unit = "USDMER05",
              description = " Value of production for individual livestock categories in USDMER05"))
}
