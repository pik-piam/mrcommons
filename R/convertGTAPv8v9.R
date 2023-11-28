#' @title convertGTAPv8v9
#' @description disaggregates country groups from GTAP according to GDP share
#' @param x unconverted magpie object from read-script
#' @param subtype GTAP header that should be read
#' @return Data as MAgPIE object with common country list
#' @author Debbora Leip, David M. Chen
#' @examples
#' \dontrun{
#' a <- readSource("convertGTAPv8v9", "81:SF01", convert = TRUE)
#' }
#' @importFrom GDPuc convertGDP
convertGTAPv8v9 <- function(x, subtype) {

  split <- toolSplitSubtype(subtype, list(version = NULL, header = NULL))

  # disaggregate to country level from GTAP regions using GDP,
  # except for trade sheets where will use FAO massbalance trade data

  if (split$header %in% c("VIWS", "VIMS", "VXWD", "VXMD")) {
    fao <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, , "dm"])
    fao <- time_interpolate(fao, interpolated_year = c(2004:2011),
                            integrate_interpolated_years = TRUE)
    fao[fao < 0] <- 0
    faoI <- dimSums(fao[, getYears(x), "import"], dim = 3)
    faoX <- dimSums(fao[, getYears(x), "export"], dim = 3)

    if (split$header %in% c("VIWS", "VIMS")) {
      w1 <- faoI
      w2 <- faoX
    } else {
      w1 <- faoX
      w2 <- faoI
    }

  } else {
    gdpMer <- calcOutput("GDPPast", GDPPast = "WDI-MI",
                         unit = "constant 2005 US$MER", aggregate = FALSE)
    gdpMer <- gdpMer[, getYears(x), , drop = TRUE]
    w1 <- GDPuc::convertGDP(gdpMer, unit_in = "constant 2005 US$MER",
                            unit_out = "current US$MER",
                            replace_NAs = "no_conversion")
    getNames(w1) <- NULL
    w2 <- w1
  }
  mapping <- toolGetMapping("regionmappingGTAP.csv", where = "mrcommons")
  x <- toolAggregate(x, rel = mapping, weight = w1,
                     from = paste0("GTAPCode", split$version),
                     to = "CountryCode", dim = 1.1)

  if ("reg2" %in% getSets(x)) {

    x <- toolAggregate(x, rel = mapping, weight = w2,
                       from = paste0("GTAPCode", split$version),
                       to = "CountryCode", dim = 1.2)
  }

  return(x)
}
