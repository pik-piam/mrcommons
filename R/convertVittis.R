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

  # Convert from "constant 2000 US$MER" to "constant 2010 US$MER"
  x <- GDPuc::convertGDP(x, "constant 2000 US$MER", "constant 2010 US$MER", replace_NAs = c("linear", 0))

  # fill missing countries with average over corresponding world region
  mapping <- toolGetMapping("regionmappingH12.csv", type = "regional")
  avg_costs <- toolAggregate(x,
                             rel = mapping[mapping[, 2] %in% getRegions(x), ],
                             from = "CountryCode",
                             to = "RegionCode",
                             weight = new.magpie(getRegions(x), getYears(x), getNames(x), 1))
  missing_countries <- setdiff(mapping[, 2], getRegions(x))
  x <- toolCountryFill(x, verbosity = 2)
  for (reg in missing_countries) {
    x[reg, , ] <- avg_costs[mapping[mapping[, 2] == reg, 3], , ]
  }

  return(x)
}
