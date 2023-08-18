#' @title convertGTAP81
#' @description disaggregates country groups from GTAP according to GDP share
#' @param x unconverted magpie object from read-script
#' @param subtype GTAP header that should be read
#' @return Data as MAgPIE object with common country list
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' a <- readSource("GTAP81", "SF01", convert = TRUE)
#' }
#'
convertGTAP81 <- function(x, subtype) {
  # optimally GDP would be converted from constant 2005 US$MER to current US$MER, but as it is used as weight
  # this shouldn't have a big impact
  gdpMer <- calcOutput("GDPPast", GDPPast = "WDI-MI", unit = "constant 2005 US$MER", aggregate = FALSE)
  gdpMer <- gdpMer[, getYears(x), , drop = TRUE]
  mapping <- toolGetMapping("regionmappingGTAP81.csv", type = "regional", where = "mappingfolder")
  x <- toolAggregate(x, rel = mapping, weight = gdpMer, from = "GTAPCode", to = "CountryCode", dim = 1)

  if ("REG2" %in% getSets(x)) {
    weight <- new.magpie(cells_and_regions = "GLO", years = getYears(gdpMer),
                         names = getItems(gdpMer, dim = 1), fill = 0)
    for (reg in getItems(gdpMer, dim = 1)) {
      weight[, , reg] <- gdpMer[reg, , ]
    }
    x <- toolAggregate(x, rel = mapping, weight = weight, from = "GTAPCode", to = "CountryCode", dim = "REG2")
  }

  return(x)
}
