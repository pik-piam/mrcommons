#' @title calcEmisNitrogenCroplandPast
#' @description calculates nitrogenous emissions from croplands in the historical period.
#'
#' @param method IPCC: emissions are calculated according the the
#' IPCC 2006 National Guidelines for Greenhouse Gas Inventories.
#' Nsurplus: Emissions in 2005 are calculated according to IPCC, and the scaled with nitrogen losses from croplands.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("EmisNitrogenCroplandPast")
#' }
#'
calcEmisNitrogenCroplandPast <- function(method = "IPCC") {

  if (method == "IPCC") {
    # deposition has to be false in budgets, otherwhise endless loop
    budget <- calcOutput("NitrogenBudgetCropland", aggregate = FALSE, deposition = "CEDS")

    fertRice <- collapseNames(
      calcOutput("FertilizerByCrop",
                 deposition = "CEDS", aggregate = FALSE)[, , "fertilizer"][, , "rice_pro"]
    )

    ef <- calcOutput("IPCCefNSoil", aggregate = FALSE)

    out <- new.magpie(getItems(budget, dim = 1), getYears(budget), getNames(ef))
    out[, , "inorg_fert"] <- collapseNames(budget[, , "fertilizer"]) * ef[, , "inorg_fert"]
    out[, , "man_crop"] <- collapseNames(budget[, , "manure_conf"]) * ef[, , "man_crop"]
    out[, , "resid"] <- dimSums(budget[, , c("ag_recycling", "bg_recycling")], dim = 3.1) * ef[, , "resid"]
    out[, , "som"] <- collapseNames(budget[, , "som"]) * ef[, , "som"]
    out[, , "rice"] <- fertRice * ef[, , "rice"]

    out <- dimOrder(out, perm = c(2, 1))


  } else if (method %in% c("Nsurplus", "Nsurplus2")) {
    # IPCC methodologies are not in line with the Nr surplus in various regions.
    # Sometimes, volatilization and leaching exceed the surplus.
    # Also, a change in NUE does not change the emission factors.
    # Emission factors in IPCC were estimated for average management conditions in 2006 and rather for global estimates.
    # Finally, the emissions only include fertilization-induced emissions,
    # not total emissions that include also the natural emissions
    #
    # In order to reach emission factors that scale with the Nr surplus, we used the following steps:
    #   - We estimate NH3 NOx NO3 and N2O emissions based on the IPCC tier 1 methodology on country level.
    #   - We add non-anthropogenic emissions as they are not included in IPCC
    #   - We add indirect emissions from atmospheric deposition, which are attribtued to the source in IPCC
    #   - We estimate globally for the year 2005 the share of denitrification as 1-(NOx,NH3,)
    #   - We calculate N2 emissions applying these global shares for N2O and N2 to the national N surplus
    #   - We rescale all emission types by one factor for each country such that the sum of all emissions
    #     is equal to the Nr surplus.
    #   - We estimate globally for the year 2005 the share of direct N2O emission in denitrification
    #     using IPCC tier 1 methodology to subdivide denitrification into N2O and N2

    baseyear <- "y2005"



    emis <- calcOutput("EmisNitrogenCroplandPast", method = "IPCC", aggregate = FALSE)

    # first iteration: calculate atmospheric deposition based on CEDS and estimate leaching
    # second iteration: calculate deposition based on Nsurplus and Oceans based on leaching
    if (method == "Nsurplus2") {
      dep <- calcOutput("AtmosphericDeposition", aggregate = FALSE, cellular = FALSE, datasource = "Nsurplus")
      budget <- calcOutput("NitrogenBudgetCropland", aggregate = FALSE, deposition = "Nsurplus")
      method <- "Nsurplus"
    } else {
      dep <- calcOutput("AtmosphericDeposition", aggregate = FALSE, cellular = FALSE, datasource = "CEDS")
      budget <- calcOutput("NitrogenBudgetCropland", aggregate = FALSE, deposition = "CEDS")
    }


    # Add indirect deposition emissions for N2O ####
    ef <- setYears(readSource("IPCC", "emissionfactors", convert = FALSE), NULL)
    emisDep <- dimSums(
                       dep[, , "crop"],
                       dim = 3) * ef[, , "ef_5"]
    emis <- add_columns(emis, addnm = "deposition", dim = 3.1)
    emis[, , "deposition"] <- 0
    emis[, , "n2o_n_direct"][, , "deposition"] <- emisDep

    # Add natural emissions ####
    emisNatural <- collapseNames(
      calcOutput("EmisNitrogenPreagriculture",
                 aggregate = FALSE, deposition = FALSE)[, , "crop"][, , c("n2_n", "accumulation"),
                                                                    invert = TRUE]
    )
    emis <- add_columns(emis, addnm = "natural", dim = 3.1)
    emis[, , "natural"] <- 0
    emis[, , "natural"] <- emisNatural[, getYears(emis), ]
    emis <- emis[, getYears(dep), ]

    # add dinitrification ####

    emisSum <- dimSums(emis, dim = c(3.1))

    n2 <- setNames(budget[, , "surplus"] - dimSums(emisSum, dim = 3), "n2_n")
    n2[n2 < 0] <- 0

    emisSum <- add_columns(emisSum, addnm = c("n2_n"), dim = 3.1)

    emisSum[, , "n2_n"] <- n2

    # Scaling emissions with surplus ####



    dentrificationShr <- setNames(dimSums(emisSum[, , c("n2o_n_direct", "n2_n")], dim = c(1, 3)) /
                                    dimSums(budget[, , "surplus"], dim = c(1, 3)), NULL)

    unscaled <- mbind(emisSum[, , c("nh3_n", "no2_n", "no3_n")],
                      setNames(dentrificationShr * budget[, , "surplus"], "denitrification"))

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

    vcat(1, "n2o emissions also occur in denitrication process. Nsurplus method should consider this")

    emissions <- add_columns(x = emissions, addnm = c("n2o_n_direct", "n2_n"), dim = 3.1)
    emisN2onShare <- sum(emisSum[, baseyear, c("n2o_n_direct")]) / sum(emissions[, baseyear, c("denitrification")])
    emissions[, , "n2o_n_direct"] <- dimSums(emissions[, , c("denitrification")], dim = 3.1) * emisN2onShare
    emissions[, , "n2_n"] <- dimSums(emissions[, , c("denitrification")], dim = 3.1) * (1 - emisN2onShare)

    out <- emissions[, , "denitrification", invert = TRUE]
    out <- add_dimension(out, nm = "cropland_soils", dim = 3.1)
  }


  return(list(
              x = out,
              weight = NULL,
              unit = "Mt Nr in various forms",
              description = "Nitrogen losses from cropland soils"))
}
