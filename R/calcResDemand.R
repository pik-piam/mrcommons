#' @title calcResDemand
#' @description Calculates the demand for Crop Residues
#'
#' @param cellular If TRUE calculation and output on cellular level
#' @param yearly whether to calculate yearly data or only magpie 5 year time steps
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @seealso
#' [calcResBiomass()]
#' @examples
#' \dontrun{
#' calcOutput("ResDemand")
#' }
#' @importFrom magclass setNames

calcResDemand <- function(cellular = FALSE, yearly = FALSE) {

  mapping <- toolGetMapping("mappingCrop2Residue.csv", where = "mrcommons", type = "sectoral")
  kres    <- findset("kres")
  past    <- findset("past_til2020")

  devStatePast <- collapseNames(calcOutput("DevelopmentState", aggregate = FALSE)[, past, "SSP2"])
  if (yearly == TRUE) {
    years        <- getYears(devStatePast, as.integer = TRUE)
    devStatePast <- time_interpolate(devStatePast, interpolated_year = c(min(years):max(years)),
                                     integrate_interpolated_years = TRUE)
  }

  if (cellular) {
    devStatePast <- toolIso2CellCountries(devStatePast)
  }

  biomass <- collapseNames(calcOutput("ResBiomass", cellular = cellular, plantparts = "ag", aggregate = FALSE))
  biomass <- toolAggregate(biomass, rel = mapping, from = "kcr", to = "kres",
                           dim = 3.1)[, ,  "res_nouse", invert = TRUE]
  if (yearly == FALSE) {
    cyears  <- intersect(intersect(getYears(biomass), past), getYears(devStatePast))
    biomass <- biomass[, cyears, ]
  } else {
    cyears  <- intersect(getYears(biomass), getYears(devStatePast))
  }

  material  <- mbind(biomass[, cyears, "res_cereals"] * (devStatePast * 0 + (1 - devStatePast) * 0.05)[, cyears, ],
                     biomass[, cyears, c("res_fibrous", "res_nonfibrous")] * 0)
  bioenergy <- biomass[, cyears, ] * (devStatePast * 0 + (1 - devStatePast) * 0.1)[, cyears, ]
  feed      <- dimSums(calcOutput("FeedPast", balanceflow = FALSE, cellular = cellular, aggregate = FALSE,
                                  yearly = yearly, nutrients = "dm")[, , kres], dim = c(3.1, 3.3))

  # attribute calculation for feed demand based on real residue mixture for each spatial unit
  # kres attributes lead to distortion of phosphorus share within each kres
  attributes <- round(collapseNames(biomass / biomass[, , "dm"]), 8)

  # NA means no residue production at all, so attributes can be put to zero
  # This will neglect residue feed demand in some areas, where it was wrongly assumend

  attributes[is.na(attributes)] <- 0

  if (any((feed != 0) & (attributes == 0))) {
    vcat(2, "Feed demand was neglected in areas, where there was no residue biomass available at all.")
  }

  feed       <- attributes[, getYears(feed), ] * feed

  production <- domesticSupply <- feed + material + bioenergy

  out        <- mbind(add_dimension(production, dim = 3.1, nm = "production"),
                      add_dimension(domesticSupply, dim = 3.1, nm = "domestic_supply"),
                      add_dimension(feed, dim = 3.1, nm = "feed"),
                      add_dimension(material, dim = 3.1, nm = "other_util"),
                      add_dimension(bioenergy, dim = 3.1, nm = "bioenergy"))

  return(list(x = out,
              weight = NULL,
              unit = "Mt DM, Nr, P, K, WM, Pj Energy",
              description = "Crop Residues Harvest and use",
              isocountries = !cellular))
}
