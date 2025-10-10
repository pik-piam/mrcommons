#' @title calcResDemand
#' @description Calculates the demand for Crop Residues
#'
#' @param cellular If TRUE calculation and output on cellular level
#' @param scenario define scenario switch for sensititvy analysis
#'                 for historical SOC budget
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcResBiomass()]
#' @examples
#' \dontrun{
#' calcOutput("ResDemand")
#' }
#' @importFrom magclass setNames

calcResDemand <- function(cellular = FALSE, scenario = "dafault") {

  mapping       <- toolGetMapping("mappingCrop2Residue.csv", where = "mrcommons", type = "sectoral")

  resCereals    <- mapping$kcr[mapping$kres == "res_cereals"]
  resFibrous    <- mapping$kcr[mapping$kres == "res_fibrous"]
  resNonfibrous <- mapping$kcr[mapping$kres == "res_nonfibrous"]

  kres           <- findset("kres")
  past           <- findset("past_til2020")

  devStatePast <- collapseNames(calcOutput("DevelopmentState", aggregate = FALSE)[, past, "SSP2"])

  if (cellular) {
    devStatePast <- toolIso2CellCountries(devStatePast)
  }

  biomass1       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                                                   plantparts = "ag", aggregate = FALSE,
                                                                   scenario = scenario)[, , resCereals]), dim = 3.1),
                                  add = "kres", nm = "res_cereals")
  biomass2       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                                                   plantparts = "ag", aggregate = FALSE,
                                                                   scenario = scenario)[, , resFibrous]), dim = 3.1),
                                  add = "kres", nm = "res_fibrous")
  biomass3       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                                                   plantparts = "ag", aggregate = FALSE,
                                                                   scenario = scenario)[, , resNonfibrous]), dim = 3.1),
                                  add = "kres", nm = "res_nonfibrous")
  biomass        <- mbind(biomass1, biomass2, biomass3)[, past, ]

  material       <- mbind(biomass[, , "res_cereals"] * (devStatePast * 0 + (1 - devStatePast) * 0.05),
                          biomass[, , c("res_fibrous", "res_nonfibrous")] * 0)
  bioenergy      <- biomass * (devStatePast * 0 + (1 - devStatePast) * 0.1)

  feed           <- dimSums(calcOutput("FeedPast", balanceflow = FALSE, cellular = cellular, aggregate = FALSE,
                                       nutrients = "dm")[, , kres], dim = c(3.1, 3.3))

  if (grepl("freeze*", scenario)) {

    biomass1       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                                                     plantparts = "ag", aggregate = FALSE,
                                                                     scenario = "default")[, , resCereals]), dim = 3.1),
                                    add = "kres", nm = "res_cereals")
    biomass2       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                                                     plantparts = "ag", aggregate = FALSE,
                                                                     scenario = "default")[, , resFibrous]), dim = 3.1),
                                    add = "kres", nm = "res_fibrous")
    biomass3       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                                                     plantparts = "ag", aggregate = FALSE,
                                                                     scenario = "default")[, , resNonfibrous]),
                                            dim = 3.1),
                                    add = "kres", nm = "res_nonfibrous")
    biomassOld     <- mbind(biomass1, biomass2, biomass3)[, past, ]

    feedshare <- toolConditionalReplace(feed / biomassOld[, , "dm"], c("is.na()", "is.infinite()"), 0)
    feedNew   <- collapseNames(feedshare * biomass[, , "dm"])
    feedNew[feedNew == 0] <- feed[feedNew == 0]
    feed      <- feedNew
  }

  # attribute calculation for feed demand based on real residue mixture for each spatial unit
  # kres attributes lead to distortion of phosphorus share within each kres
  attributes <- round(collapseNames(biomass / biomass[, , "dm"]), 8)

  # NA means no residue production at all, so attributes can be put to zero
  # This will neglect residue feed demand in some areas, where it was wrongly assumend

  attributes[is.na(attributes)] <- 0

  if (any((feed != 0) & (attributes == 0))) {
    vcat(2, "Feed demand was neglected in areas, where there was no residue biomass available at all.")
  }

  feed       <- attributes * feed

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
