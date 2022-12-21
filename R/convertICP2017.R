#' @title convertICP2017
#' @description  converts data of World Bank ICP round, downloaded from here: https://databank.worldbank.org/source/icp-2017
#' mainly a currency conversion to MER05. Assume that in the original dataset, 2011 values are in 2011MER and 2017 in 2017MER, given the ICP rounds
#'
#' @param subtype data subtype needed. Either "priceLevel", or "per_cap_exp"
#' @param x MAgPIE object containing original values
#' @return magpie object of relative price levels (world = 100) or per capita expenditure (USD17 MER)
#' @author David M Chen
#' @examples
#' \dontrun{
#' a <- convertSource("ICP2017", "per_cap_exp")
#' }
#'
convertICP2017 <- function(x, subtype = "per_cap_expMER") {
  # currency convert for expenditures
  if (subtype == "per_cap_expMER") {

    x2011 <- GDPuc::convertGDP(x[, 2011, ], unit_in = "constant 2011 US$MER", unit_out = "constant 2005 US$MER")
    x2017 <- GDPuc::convertGDP(x[, 2017, ], unit_in = "constant 2017 US$MER", unit_out = "constant 2005 US$MER")
    x <- mbind(x2011, x2017)
    x[is.na(x)] <- 0
  }

  # currency convert for expenditures
  if (subtype == "per_cap_expPPP") {

    x2011 <- GDPuc::convertGDP(x[, 2011, ], unit_in = "constant 2011 US$PPP", unit_out = "constant 2005 US$MER")
    x2017 <- GDPuc::convertGDP(x[, 2017, ], unit_in = "constant 2017 US$PPP", unit_out = "constant 2005 US$MER")
    x <- mbind(x2011, x2017)
    x[is.na(x)] <- 0
  }

  # remove aggregate categories

  x <- x[, , c("GROSS DOMESTIC PRODUCT",
             "FOOD AND NON-ALCOHOLIC BEVERAGES",
             "ALCOHOLIC BEVERAGES, TOBACCO AND NARCOTICS",
             "HOUSEHOLDS AND NPISHS FINAL CONSUMPTION EXPENDITURE",
             "INDIVIDUAL CONSUMPTION EXPENDITURE BY HOUSEHOLDS WITHOUT HOUSING",
             "GENERAL GOVERNMENT FINAL CONSUMPTION EXPENDITURE"), inv = TRUE]


  return(x)
}
