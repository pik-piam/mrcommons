#' @title convertVittis
#' @description Aggregate production costs from Vittis dataset to MAgPIE crop
#' categories and change unit from US$2000 to US$2005.
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing national-scale costs of production for
#' 10 crops, disaggregated in 9 distinct cost elements
#' @author Debbora Leip
#' @importFrom GDPuc convertGDP

convertVittis <- function(x) {
  # map to MAgPIE categories with global crop areas as weights
  mapping <- toolGetMapping("VittisCropCategories.csv", type = "sectoral", where = "mrcommons")
  cropArea <- calcOutput("Croparea", sectoral = "ProductionItem", aggregate = "GLO")
  weights <- toolAggregate(cropArea[, "y2000", unique(mapping[, "ProductionItem"])], mapping,
                           from = "ProductionItem", to = "Vittis", dim = 3)
  x <- toolAggregate(x, mapping, weight = weights, from = "Vittis", to = "kcr", dim = 3.2)

  # convert US$2000 to US$2005
  x <- convertGDP(x, unit_in = "constant 2000 Int$PPP",  unit_out = "constant 2005 Int$PPP")

  # fill missing countries with average over corresponding world region
  mapping <- toolGetMapping("regionmappingH12.csv", type = "regional")
  avgCosts <- toolAggregate(x, rel = mapping[mapping[, 2] %in% getItems(x, dim = 1), ], from = "CountryCode",
                            to = "RegionCode", weight = new.magpie(getItems(x, dim = 1), getYears(x), getNames(x), 1))
  missingCountries <- setdiff(mapping[, 2], getItems(x, dim = 1))
  x <- toolCountryFill(x, verbosity = 2)
  for (reg in missingCountries) {
    x[reg, , ] <- avgCosts[mapping[mapping[, 2] == reg, 3], , ]
  }

  return(x)
}
