#' @title calcManureRecyclingCroplandPast
#' @description calculates manure recycling to cropland based on excretions, animal waste management types
#' (and their shares per country) and emission factors for nitrogenous emissions in livestock confinements
#'
#' @param cellular if TRUE value is calculate and returned (set aggregate to FALSE!) on cellular level
#' @param products "sum" (default) or "kli"
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @seealso
#' [calcExcretion()]
#' @examples
#' \dontrun{
#' calcOutput("calcManureRecyclingCroplandPast")
#' }
#' @importFrom magclass getNames<-


calcManureRecyclingCroplandPast <- function(products = "sum", cellular = FALSE) {

  past               <- findset("past")
  excretion          <- collapseNames(calcOutput("Excretion", cellular = cellular, attributes = "npkc",
                                                 aggregate = FALSE)[, past, "confinement"])
  emissionFactorsN  <- calcOutput("EF3confinement", selection = "recycling", aggregate = FALSE)
  lossRatesC        <- calcOutput("ClossConfinement", aggregate = FALSE)
  animalWasteMSShare <- collapseNames(calcOutput("AWMSconfShr", aggregate = FALSE)[, past, "constant"])

  if (cellular) {

    emissionFactorsN  <- toolIso2CellCountries(emissionFactorsN)
    lossRatesC        <- toolIso2CellCountries(lossRatesC)
    animalWasteMSShare <- toolIso2CellCountries(animalWasteMSShare)
  }

  if (products == "sum") {

    manureNitrogen       <- dimSums(excretion[, , "nr"] * animalWasteMSShare * emissionFactorsN, dim = c(3.1, 3.3))
    vcat(verbosity = 2, "no P and K losses in manure management assumed")
    manurePhosphorKalium <- dimSums(excretion[, , c("p", "k")], dim = 3.1)
    manureCarbon         <- dimSums(excretion[, , "c"] * animalWasteMSShare * (1 - lossRatesC), dim = c(3.1, 3.3))

  } else if (products == "kli") {

    manureNitrogen       <- dimSums(excretion[, , "nr"] * animalWasteMSShare * emissionFactorsN, dim = c(3.3))
    vcat(verbosity = 2, "no P and K losses in manure management assumed")
    manurePhosphorKalium <- excretion[, , c("p", "k")]
    manureCarbon         <- dimSums(excretion[, , "c"] * animalWasteMSShare * (1 - lossRatesC), dim = c(3.3))

  } else {
    stop(paste("Type", products, "is not a valid for parameter 'products'."))
  }

  out       <- mbind(manureNitrogen, manurePhosphorKalium, manureCarbon)

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mt Nr, P, K, C",
              description  = "Manure from confinements recycled to croplands",
              isocountries = !cellular)
  )
}
