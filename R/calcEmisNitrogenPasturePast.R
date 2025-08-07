#' @title calcEmisNitrogenPasturePast
#' @description
#' Calculates nitrogenous emissions from pastures for the historical period
#'
#' @param method IPCC: emissions are calculated according the the IPCC 2006 National Guidelines
#'               for Greenhouse Gas Inventories. Nsurplus: Emissions in 2005 are calculated according to IPCC,
#'               and the scaled with nitrogen losses from croplands.
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcEmisNitrogenPast()],
#' [calcExcretion()]
#' @examples
#' \dontrun{
#' calcOutput("EmisNitrogenPasturePast")
#' }
#'
calcEmisNitrogenPasturePast <- function(method = "IPCC") {

  if (method == "IPCC") {
    budget <- calcOutput("NitrogenBudgetPasture", aggregate = FALSE, deposition = "CEDS")
    excretion <- dimSums(calcOutput("Excretion", aggregate = FALSE)[, , c("grazing", "stubble_grazing")][, , "nr"],
                         dim = c(3.1, 3.3))
    # add emissions from fertilizer on pasture
    ef <- setYears(readSource("IPCC", "emissionfactors", convert = FALSE), NULL)
    ef3Prp <- setYears(calcOutput("EF3prp", aggregate = FALSE), NULL)

    out <- dimSums(excretion * ef3Prp, dim = 3.1)
    out <- add_dimension(out, nm = "pasture_soils", dim = 3.1)


  } else if (method %in% c("Nsurplus", "Nsurplus2")) {

    baseyear <- "y2005"

    # first iteration: calculate atmospheric deposition based on CEDS and estimate leaching
    # second iteration: calculate deposition based on Nsurplus and Oceans based on leaching
    if (method == "Nsurplus2") {
      dep <- calcOutput("AtmosphericDeposition", aggregate = FALSE, cellular = FALSE, datasource = "Nsurplus")
      budget <- calcOutput("NitrogenBudgetPasture", aggregate = FALSE, deposition = "Nsurplus")
      method <- "Nsurplus"
    } else {
      dep <- calcOutput("AtmosphericDeposition", aggregate = FALSE, cellular = FALSE, datasource = "CEDS")
      budget <- calcOutput("NitrogenBudgetPasture", aggregate = FALSE, deposition = "CEDS")
    }

    # anthropogenic emissions ####
    emis <- calcOutput("EmisNitrogenPasturePast",
                       method = "IPCC",
                       aggregate = FALSE)[, getYears(dep), ]

    # Add indirect deposition emissions for N2O ####
    ef <- setYears(readSource("IPCC", "emissionfactors", convert = FALSE), NULL)
    emisDep <- dimSums(
                       dep[, , "past"],
                       dim = 3) * ef[, , "ef_5"]
    emis[, , "n2o_n_direct"] <- emis[, , "n2o_n_direct"] + emisDep

    # Add natural emissions ####
    emisNatural <- collapseNames(
      calcOutput("EmisNitrogenPreagriculture", aggregate = FALSE, deposition = FALSE)
      [, , "past"][, , c("n2_n", "accumulation"), invert = TRUE]
    )
    cyears <- intersect(getYears(emis), getYears(emisNatural))
    emis <- emis[, cyears, ] + emisNatural[, cyears, ]

    # add dinitrification ####
    cyears <- intersect(getYears(emis), getYears(budget))
    emisSum <- dimSums(emis, dim = c(3.1))[, cyears, ]
    n2 <- setNames(budget[, cyears, "surplus"] -
                     dimSums(emisSum[, cyears, ], dim = 3), "n2_n")
    n2[n2 < 0] <- 0

    emisSum <- add_columns(emisSum, addnm = c("n2_n"), dim = 3.1)

    emisSum[, , "n2_n"] <- n2


    # Scaling emissions with surplus ####
    denitrificationShr <- setNames(dimSums(emisSum[, cyears, c("n2o_n_direct", "n2_n")],
                                           dim = c(1, 3)) /
                                     dimSums(budget[, cyears, "surplus"], dim = c(1, 3)), NULL)
    unscaled <- mbind(emisSum[, cyears, c("nh3_n", "no2_n", "no3_n")],
                      setNames(denitrificationShr * budget[, cyears, "surplus"], "denitrification"))

    emissionShares <- setYears(unscaled[, baseyear, ] / dimSums(unscaled[, baseyear, ], dim = 3), NULL)
    emissionSharesGlo <- setYears(dimSums(unscaled[, baseyear, ], dim = 1, na.rm = TRUE) /
                                    dimSums(unscaled[, baseyear, ], dim = c(1, 3), na.rm = TRUE), NULL)
    emissionShares[which(dimSums(emisSum[, , ], dim = c(2, 3)) < 0.001), , ] <- emissionSharesGlo

    # distributing cropland surplus according to these shares.
    emissions <- collapseNames(budget[, , "surplus"]) * emissionShares

    # assume globally same fraction of N2O in dentrification process.
    # For comparison: Bessou et al comes to approximately 11%.
    # Bessou, C., B. Mary, J. Léonard, M. Roussel, E. Gréhan, and B. Gabrielle. 2010.
    # “Modelling Soil Compaction Impacts on Nitrous Oxide Emissions in Arable Fields.”
    # European Journal of Soil Science 61 (3): 348–63. doi:10.1111/j.1365-2389.2010.01243.x.
    emissions <- add_columns(x = emissions, addnm = c("n2o_n_direct", "n2_n"), dim = 3.1)
    emisN2onShare <- sum(emisSum[, baseyear, c("n2o_n_direct")]) / sum(emissions[, baseyear, c("denitrification")])
    emissions[, , "n2o_n_direct"] <- dimSums(emissions[, , c("denitrification")], dim = 3.1) * emisN2onShare
    emissions[, , "n2_n"] <- dimSums(emissions[, , c("denitrification")], dim = 3.1) * (1 - emisN2onShare)

    out <- emissions[, , "denitrification", invert = TRUE]
    out <- add_dimension(out, nm = "pasture_soils", dim = 3.1)
  }


  return(list(
              x = out,
              weight = NULL,
              unit = "Mt Nr in various forms",
              min = 0,
              description = "Nitrogen emissions from pasture soils"))
}
