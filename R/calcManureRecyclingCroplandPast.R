#' @title calcManureRecyclingCroplandPast
#' @description calculates manure recycling to cropland based on excretions, animal waste management types
#' (and their shares per country) and emission factors for nitrogenous emissions in livestock confinements
#'
#' @param cellular if TRUE value is calculate and returned (set aggregate to FALSE!) on cellular level
#' @param cells Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param products "sum" (default) or "kli"
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#'
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @seealso
#' [calcExcretion()]
#' @examples
#' \dontrun{
#' calcOutput("ManureRecyclingCroplandPast")
#' }
calcManureRecyclingCroplandPast <- function(products = "sum", cellular = FALSE, cells = "lpjcell") {
  excretion          <- collapseNames(calcOutput("Excretion", cellular = cellular, cells = cells, attributes = "npkc",
                                                 aggregate = FALSE)[, , "confinement"])
  emissionFactorsN   <- calcOutput("EF3confinement", selection = "recycling", aggregate = FALSE)
  lossRatesC         <- calcOutput("ClossConfinement", aggregate = FALSE)
  animalWasteMSShare <- collapseNames(calcOutput("AWMSconfShr", aggregate = FALSE)[, , "constant"])

  cyears <- intersect(getYears(animalWasteMSShare), getYears(excretion))
  excretion <- excretion[, cyears, ]
  animalWasteMSShare <- animalWasteMSShare[, cyears, ]

  if (cellular) {
    countries <- getItems(excretion, dim = ifelse(cells == "lpjcell", 1.3, 1.1))

    emissionFactorsN   <- emissionFactorsN[countries, , ]
    lossRatesC         <- lossRatesC[countries, , ]
    animalWasteMSShare <- animalWasteMSShare[countries, , ]
  }

  if (products == "sum") {
    manureNitrogen       <- dimSums(excretion[, , "nr"] * animalWasteMSShare * emissionFactorsN, dim = c(3.1, 3.3))
    vcat(verbosity = 2, "no P and K losses in manure management assumed")
    manurePhosphorKalium <- dimSums(excretion[, , c("p", "k")], dim = 3.1)
    manureCarbon         <- dimSums(excretion[, , "c"] * animalWasteMSShare * (1 - lossRatesC), dim = c(3.1, 3.3))
  } else if (products == "kli") {
    manureNitrogen       <- dimSums(excretion[, , "nr"] * animalWasteMSShare * emissionFactorsN, dim = 3.3)
    vcat(verbosity = 2, "no P and K losses in manure management assumed")
    manurePhosphorKalium <- excretion[, , c("p", "k")]
    manureCarbon         <- dimSums(excretion[, , "c"] * animalWasteMSShare * (1 - lossRatesC), dim = 3.3)
  } else {
    stop(paste("Type", products, "is not a valid for parameter 'products'."))
  }

  out <- mbind(manureNitrogen, manurePhosphorKalium, manureCarbon)

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mt Nr, P, K, C",
              description  = "Manure from confinements recycled to croplands",
              isocountries = !cellular)
  )
}
