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
#'
convertGTAPv8v9 <- function(x, subtype) {

  split <- toolSplitSubtype(subtype, list(version = NULL, header = NULL))

  # disaggregate to country level from GTAP regions using GDP,
  # except for trade sheets where will use FAO massbalance trade data

  mapping <- toolGetMapping("regionmappingGTAP.csv", where = "mrcommons")

  if (split$header %in% c("VIWS", "VIMS", "VXWD", "VXMD")) {

    cat("Note: Reading specific sheets (only used for calcGTAPTrade at the moment),
         where disaggregation from GTAP to MAgPIE regions does not happen 'as expected':
         all countries within a GTAP region get the same absolute value
         (for later calculation of relative value)")

    sectorMapping <- toolGetMapping(type = "sectoral", name = "mappingGTAPMAgPIETrade.csv", where = "mrland")
    sectorMapping <- sectorMapping[which(sectorMapping$gtap != "xxx" & sectorMapping$magpie != "zzz"), ]

    fao <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, , "dm"])
    fao <- time_interpolate(fao, interpolated_year = c(2004:2011),
                            integrate_interpolated_years = TRUE)
    fao[fao < 0] <- 0

    fao <- toolAggregate(fao, rel = mapping, from = "CountryCode",
                         to = paste0("GTAPCode", split$version), dim = 1, partrel = TRUE)

    faoI <- toolAggregate(collapseNames(fao[, getYears(x), "import"]),
                          rel = sectorMapping, from = "magpie", to = "gtap",
                          dim = 3, partrel = TRUE)

    faoX <-  toolAggregate(collapseNames(fao[, getYears(x), "export"]),
                           rel = sectorMapping, from = "magpie", to = "gtap",
                           dim = 3, partrel = TRUE)

    if (split$header %in% c("VIWS", "VIMS")) {
      w1 <- faoI + 10^-10
      w2 <- faoX  + 10^-10
    } else {
      w1 <- faoX + 10^-10
      w2 <- faoI + 10^-10
    }


    x <- toolAggregate(x[, ,  getNames(w1)], rel = mapping, weight = w1,
                       from = paste0("GTAPCode", split$version),
                       to = "CountryCode", dim = 1.1)

    if ("reg2" %in% getSets(x)) {

      x <- toolAggregate(x[, ,  getNames(w2)], rel = mapping, weight = w2,
                         from = paste0("GTAPCode", split$version),
                         to = "CountryCode", dim = 1.2)
    }

  } else {
    gdpMer <- calcOutput("GDPPast", years = getYears(x), aggregate = FALSE)
    w1 <- GDPuc::toolConvertGDP(gdpMer,
                                unit_in = "constant 2017 Int$PPP",
                                unit_out = "current US$MER",
                                replace_NAs = "no_conversion")
    getNames(w1) <- NULL
    w2 <- w1

    x <- toolAggregate(x, rel = mapping, weight = w1,
                       from = paste0("GTAPCode", split$version),
                       to = "CountryCode", dim = 1.1)

    if ("reg2" %in% getSets(x)) {

      x <- toolAggregate(x, rel = mapping, weight = w2,
                         from = paste0("GTAPCode", split$version),
                         to = "CountryCode", dim = 1.2)
    }
  }

  return(x)
}
