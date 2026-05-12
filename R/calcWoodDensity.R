#' @title calcWoodDensity
#' @description Calculates country-level basic wood density (tDM per m3) based on
#' IPCC 2006 Vol 4 Chapter 12 Table 12.4 values and Koeppen-Geiger climate classification.
#' Density values: Tropical (A) = 0.59, Arid (B) = 0.52, Temperate (C) = 0.45,
#' Continental (D) = 0.45, Polar (E) = 0.45.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("WoodDensity")
#' }
#' @importFrom magclass new.magpie getNames getCells getYears setYears dimSums
#' @importFrom madrat toolAggregate toolCountryFill
#' @export

calcWoodDensity <- function() {

  # IPCC basic wood density by Koeppen-Geiger major climate zone (tDM per m3)
  # Source: IPCC 2006, Vol 4, Chapter 12, Table 12.4
  ipccDensity <- c(A = 0.59, B = 0.52, C = 0.45, D = 0.45, E = 0.45)

  # Get cell-level Koeppen-Geiger climate class shares
  climateClass <- calcOutput("ClimateClass", datasource = "koeppen", aggregate = FALSE)

  # Use first year only (shares are time-invariant)
  if (!is.null(getYears(climateClass))) {
    climateClass <- setYears(climateClass[, 1, ], NULL)
  }

  # Normalize climate class shares to sum to 1 per spatial unit
  total <- dimSums(climateClass, dim = 3)
  climateClass <- climateClass / total
  climateClass[is.na(climateClass)] <- 0

  # Map 31 Koeppen classes to major zones (first letter) and assign density
  majorZone <- substr(getNames(climateClass), 1, 1)
  densityVec <- ipccDensity[majorZone]
  densityMag <- new.magpie(cells_and_regions = "GLO", years = NULL, names = getNames(climateClass),
                           fill = densityVec)

  # Weighted average density: sum(share_clcl * density_clcl) per spatial unit
  x <- dimSums(climateClass * densityMag, dim = 3)
  x <- toolCountryFill(x, fill = 0.5)

  return(list(x = x,
              weight = NULL,
              min = 0,
              unit = "tDM per m3",
              description = "Basic wood density based on IPCC 2006 and Koeppen-Geiger climate classification"))
}
