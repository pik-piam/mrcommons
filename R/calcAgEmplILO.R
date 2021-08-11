#' @title calcAgEmplILO
#' @description calculates complete dataset of number of people employed in agriculture, forestry and fishery based
#' on ILO modelles dataset and GDPpcPPP05 for regression
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("AgEmplILO")
#' }
#' @importFrom magclass getNames<- getYears where

calcAgEmplILO <- function() {
  iloEmpl <- readSource("ILOSTAT", "EmplByActivityModelled")[, , list("Total", "Aggregate: Agriculture"), drop = TRUE]
  getNames(iloEmpl) <- "Agriculture, forestry and fishing"
  iloEmpl[iloEmpl == 0] <- NA

  # from thousands to millions
  iloEmpl <- iloEmpl / 1000

  # calculate estimates of people employed in agriculture for missing countries
  # regression: sqrt(share s of total population that is employed in agriculture) ~ log10(GDP PPP per capita)
  regCoeff <- readSource("RegressionsILO", "AgEmpl")

  gdp <- calcOutput("GDPpppPast", subtype = "IHME_USD05_PPP_pc_completed", aggregate = FALSE) # mio. USD$PPP05
  pop <- calcOutput("PopulationPast", aggregate = FALSE) # mio. people
  years <- intersect(getYears(gdp), getYears(pop))
  GDPpc <- gdp[, years, , drop = TRUE] / pop[, years, , drop = TRUE] # GDPpcPPP05

  estShare <- (regCoeff[, , "slope", drop = TRUE] * log10(GDPpc) + regCoeff[, , "intercept", drop = TRUE]) ** 2
  const <- (regCoeff[, , "slope"] * log10(regCoeff[, , "threshold"]) + regCoeff[, , "intercept"]) ** 2
  estShare[GDPpc > regCoeff[, , "threshold"]] <- const

  estEmpl <- estShare * pop[, years, , drop = TRUE]

  # reduce years to those with estimations, fill missing countries
  regions <- where(!is.finite(iloEmpl))$true$regions
  years <- intersect(getYears(iloEmpl), years)
  iloEmpl <- iloEmpl[, years, ]
  iloEmpl[regions, , ] <- estEmpl[regions, years, ]

  # weight for aggregation to world regions
  weight <- NULL

  return(list(x = iloEmpl,
              weight = weight,
              unit = "mio. people",
              description = "Employment in agriculture, forestry and fishery (based on ILO modelled estimates)"))
}
