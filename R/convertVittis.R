#' @title convertVittis
#' @description Aggregate production costs from Vittis dataset to MAgPIE crop
#' categories and change unit from US$2000 to US$2005.
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing national-scale costs of production for
#' 10 crops, disaggregated in 9 distinct cost elements
#' @author Debbora Leip

convertVittis <- function(x) {
  # map to MAgPIE categories with global crop areas as weights
  mapping <- toolGetMapping("VittisCropCategories.csv", type = "sectoral", where = "mrcommons")
  weights <- toolAggregate(
    calcOutput("Croparea", sectoral = "ProductionItem", aggregate = "GLO")
    [, "y2000", unique(mapping[, "ProductionItem"])], mapping, from = "ProductionItem", to = "Vittis", dim = 3)
  x <- toolAggregate(x, mapping, weight = weights, from = "Vittis", to = "kcr", dim = 3.2)

  # convert US$2000 to US$2005
  pppCurrent <- readSource("WDI", subtype = "NY.GDP.MKTP.PP.CD")
  ppp2011 <- readSource("WDI", "NY.GDP.MKTP.PP.KD") # 2011 ppp dataset

  pppRatio2011to2005 <- setYears(pppCurrent["USA", "y2005", ] / ppp2011["USA", "y2005", ], NULL)
  pppRatio2011to2000 <- setYears(pppCurrent["USA", "y2000", ] / ppp2011["USA", "y2000", ], NULL)
  pppRatio2000to2005 <- collapseDim(pppRatio2011to2005 / pppRatio2011to2000, dim = c(1, 3))

  x <- x * pppRatio2000to2005

  # fill missing countries with average over corresponding world region
  mapping <- toolGetMapping("regionmappingH12.csv", type = "regional")
  avgCosts <- toolAggregate(
    x, rel = mapping[mapping[, 2] %in% getItems(x, dim = 1), ],
    from = "CountryCode", to = "RegionCode", weight = new.magpie(getItems(x, dim = 1), getYears(x), getNames(x), 1))
  missingCountries <- setdiff(mapping[, 2], getItems(x, dim = 1))
  x <- toolCountryFill(x, verbosity = 2)
  for (reg in missingCountries) {
    x[reg, , ] <- avgCosts[mapping[mapping[, 2] == reg, 3], , ]
  }

  return(x)
}
