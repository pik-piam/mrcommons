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

  weights <- calcOutput("Croparea",
                        sectoral = "ProductionItem",
                        aggregate = "GLO")[, "y2000", unique(mapping[, "ProductionItem"])] %>%
    toolAggregate(mapping, from = "ProductionItem", to = "Vittis", dim = 3)
  x <- toolAggregate(x, mapping, weight = weights, from = "Vittis", to = "kcr", dim = 3.2)

  # Convert from "constant 2000 Int$PPP" to "constant 2005 Int$PPP"
  x <- GDPuc::convertGDP(x, "constant 2000 Int$PPP", "constant 2005 Int$PPP", replace_NAs = c("linear", 0))

  # fill missing countries with average over corresponding world region
  mapping <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
  avgCosts <- toolAggregate(x,
                             rel = mapping[mapping[, 2] %in% getItems(x, dim = 1), ],
                             from = "CountryCode",
                             to = "RegionCode",
                             weight = new.magpie(getItems(x, dim = 1), getYears(x), getNames(x), 1))
  missingCountries <- setdiff(mapping[, 2], getItems(x, dim = 1))

  x <- toolCountryFill(x, verbosity = 2)
  for (reg in missingCountries) {
    x[reg, , ] <- avgCosts[mapping[mapping[, 2] == reg, 3], , ]
  }

  return(x)
}
