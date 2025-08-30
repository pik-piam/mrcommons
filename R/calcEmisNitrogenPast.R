#' @title calcEmisNitrogenPast
#' @description
#' Calculates nitrogenous emissions from all emission sources for the historical period.
#' Complements own estimates with Edgar esimtates for the historical period.
#' @param method IPCC: emissions are calculated according the the IPCC 2006 National Guidelines
#'               for Greenhouse Gas Inventories. Nsurplus: Emissions in 2005 are calculated according to IPCC,
#'               and the scaled with nitrogen losses from croplands.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("EmisNitrogenPast")
#' }
#'
calcEmisNitrogenPast <- function(method = "IPCC") {
  # add CEDS past
  inventory <- calcOutput("EmissionInventory", aggregate = FALSE, datasource = "CEDS", targetResolution = "sectoral",
                          from = "CEDS59", to = "Sectors")
  vcat(verbosity = 1, "CEDS emissions are not complete yet, and the units have to be controlled.")

  getNames(inventory, dim = 2) <- sub(getNames(inventory, dim = 2), pattern = "n2o_n", replacement = "n2o_n_direct")

  # add other emission forms
  allemis <- c("n2o_n_direct", "n2o_n_indirect", "nh3_n", "no2_n", "no3_n")
  addemis <- setdiff(allemis, getNames(inventory, dim = 2))

  if (length(addemis) > 0) {
    inventory <- add_columns(inventory, dim = 3.2, addnm = addemis)
  }
  inventory <- inventory[, , allemis]


  if (method == "IPCC") {
    ### Add own estimates
    cropland <- calcOutput("EmisNitrogenCroplandPast", method = method, aggregate = FALSE)
    pasture <- calcOutput("EmisNitrogenPasturePast", method = method, aggregate = FALSE)
    pasture <- pasture[, getYears(cropland), ]
    awms <- add_dimension(
                          x = dimSums(calcOutput("EmisNitrogenAWMSPast", aggregate = FALSE),
                                      dim = c(3.1, 3.2))[, , getNames(cropland, dim = 2)], dim = 3.1, nm = "awms")
    awms <- awms[, getYears(cropland), ]
    emisAg <- mbind(
      cropland,
      pasture,
      awms
    )

    # add indirect emission
    emisAg <- add_columns(emisAg, dim = 3.2, addnm = c("n2o_n_indirect"))
    ef <- setYears(readSource("IPCC", "emissionfactors", convert = FALSE), NULL)
    emisAg[, , "n2o_n_indirect"] <- collapseNames(emisAg[, , "nh3_n"] * ef[, , "ef_5"]
                                                  + dimSums(emisAg[, , c("nh3_n", "no2_n")],
                                                            dim = 3.2) * ef[, , "ef_4"])

    inventory <- mbind(inventory, emisAg)
    inventory <- inventory[, , "agriculture", invert = TRUE]


    inventory[is.na(inventory)] <- 0
  } else if (method %in% c("Nsurplus", "Nsurplus2")) {
    ### Add own estimates
    cropland <- calcOutput("EmisNitrogenCroplandPast", method = method, aggregate = FALSE)
    pasture <- calcOutput("EmisNitrogenPasturePast", method = method, aggregate = FALSE)
    awms <- add_dimension(
                          x = dimSums(calcOutput("EmisNitrogenAWMSPast", aggregate = FALSE),
                                      dim = c(3.1, 3.2))[, , getNames(cropland, dim = 2)], dim = 3.1, nm = "awms")
    awms <- awms[, getYears(cropland), ]
    natural <- calcOutput("EmisNitrogenNonaglandPast", method = method, aggregate = FALSE)
    oceans <- calcOutput("EmisNitrogenOceans", method = method, aggregate = FALSE)
    oceans <- oceans[, getYears(cropland), ]
    water <- calcOutput("EmisNitrogenWater", method = method, aggregate = FALSE)
    water <- water[, getYears(cropland), ]


    emisAg <- mbind(
      cropland,
      pasture,
      awms,
      natural,
      water,
      oceans
    )

    inventory <- mbind(inventory, emisAg)
    inventory <- inventory[, , "agriculture", invert = TRUE]
    inventory[is.na(inventory)] <- 0
    inventory <- add_columns(inventory, dim = 3.1, addnm = c("lightning"))
    inventory[, , "lightning"] <- 0
    # assuming 5 Tg of Nr from lightning, based on Lamarque et al 2013.
    vcat(2, "lightning emissions attributed to Antarctica.")
    inventory["ATA", , "lightning"][, , "no2_n"] <- 5

  }
  return(list(
              x = inventory,
              weight = NULL,
              unit = "Mt N2O-N, NH3-N, NO2-N, NO3-N, N2-N",
              description = "(Not yet) Complete emission inventory for nitrogen emissions
                   including were possible own estimates for the past. n2o_n_direct
                   and n2o_n_indirect are subcategories of n2o_n."))
}
