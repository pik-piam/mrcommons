#' @title calcVoP_livst
#' @description Calculates the value of production of individual livestock categories
#'
#' @param other boolean: should FAO livestock categories that can't be matched to MAgPIE categories (i.e. beeswax, wool,
#' silkworms, and honey) be reported as "livst_other"?
#' @return magpie object. in mio. USDMER05
#' @author Debbora Leip
#' @importFrom GDPuc convertGDP
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("VoP_livst")
#' }
#'
calcVoP_livst <- function(other = FALSE) {

  # Value of production of individual items (current US$MER -> US$MER05)
  item <- "Gross_Production_Value_(current_thousand_US$)_(1000_US$)"
  VoPcurrentMER <- readSource("FAO_online", "ValueOfProd")[, , item] / 1000 # mio. current US$MER
  VoP <- convertGDP(VoPcurrentMER,
                    unit_in = "current US$MER",
                    unit_out = "constant 2005 US$MER")
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

  return(list(x = VoPlivst,
              weight = NULL,
              unit = "USDMER05",
              description = " Value of production for individual livestock categories in USDMER05"))
}
