#' Read Eurostat historical emissions (env_air_gge)
#'
#' @param x MAgPIE object to be converted
#' @param subtype 'emissions' for original Eurostat emissions split,
#' 'MACCemi' for MACC historical emissions, or 'sectorEmi' for sector specific
#' emissions, or 'latest' for most up-to-date data
#' @return A MAgPIE object containing the Eurostat historical emissions (MtCO2)
#' @author Renato Rodrigues
#' @examples
#' \dontrun{
#' a <- convertEurostat(x, subtype = "emissions")
#' }
#'
convertEurostat <- function(x, subtype) {
  switch(subtype,
    "emissions" = toolCountryFill(x, fill = NA, verbosity = 2),
    "sectorEmi" = convertEurostatSectorEmi(x),
    "MACCemi" = convertEurostatMACCemi(x),
    "latest" = toolCountryFill(x, fill = NA, verbosity = 2),
    stop("Bad input for convertEurostat. Invalid 'subtype' argument.")
  )
}

######################################################################################
# Functions
######################################################################################
convertEurostatSectorEmi <- function(x) {
  x <- toolCountryFill(x, fill = NA, verbosity = 2)
  # mapping eurostat to sector emissions
  mapping <- list(
    "power" = list("energy" = c("Fuel combustion in public electricity and heat production")),
    "refining" = list("energy" = c("Fuel combustion in petroleum refining")),
    "solids" = list("energy" = c(
      "Fuel combustion in manufacture of solid fuels and other energy industries",
      "Other fuel combustion sectors n_e_c_"
    )),
    "extraction" = list("process" = c(
      "Solid fuels - fugitive emissions",
      "Oil, natural gas and other energy production - fugitive emissions"
    )),
    "build" = list("energy" = c(
      "Fuel combustion in commercial and institutional sector",
      "Fuel combustion by households"
    )),
    "indst" = list(
      "energy" = c("Fuel combustion in manufacturing industries and construction"),
      "process" = c("Industrial processes and product use")
    ),
    "trans" = list("energy" = c("Fuel combustion in transport")),
    "bunkers" = list("energy" = c("International bunkers (memo item)")),
    "agriculture" = list(
      "energy" = c("Fuel combustion in agriculture, forestry and fishing"),
      "process" = c("Agriculture")
    ),
    "waste" = list("process" = c("Waste management")),
    "lulucf" = list("process" = c("Land use, land use change, and forestry (LULUCF)")),
    "cdr" = list("process" = c("Transport and storage of CO2 (memo item)")),
    "other" = list(
      "energy" = c("Multilateral operations (memo item)"),
      "process" = c("Other sectors")
    ),
    "indirect" = list("process" = c("Indirect CO2"))
  )

  x <- mbind(lapply(names(mapping), function(var) {
    mbind(lapply(names(mapping[[var]]), function(t) {
      add_dimension(
        add_dimension(
          dimSums(x[, , mapping[[var]][[t]]], na.rm = TRUE, dim = 3.2),
          dim = 3.2, add = "sector", nm = var
        ),
        dim = 3.3, add = "type", nm = t
      )
    }))
  }))
}

convertEurostatMACCemi <- function(x) {
  x <- toolCountryFill(x, fill = NA, verbosity = 2)
  # mapping eurostat to MACC emissions
  mapping <- list(
    "ch4coal" = list("emi" = "CH4", "accounts" = c("Solid fuels - fugitive emissions")),
    "ch4wstl" = list("emi" = "CH4", "accounts" = c("Solid waste disposal")),
    "ch4wsts" = list("emi" = "CH4", "accounts" = c("Wastewater treatment and discharge")),
    "ch4rice" = list("emi" = "CH4", "accounts" = c("Rice cultivation")),
    "ch4animals" = list("emi" = "CH4", "accounts" = c("Enteric fermentation")),
    "ch4anmlwst" = list("emi" = "CH4", "accounts" = c("Manure management")),
    "ch4agwaste" = list("emi" = "CH4", "accounts" = c("Field burning of agricultural residues")),
    "n2otrans" = list("emi" = "N2O", "accounts" = c("Fuel combustion in transport")),
    "n2oadac" = list("emi" = "N2O", "accounts" = c("Adipic acid production")),
    "n2onitac" = list("emi" = "N2O", "accounts" = c("Nitric acid production")),
    "n2oagwaste" = list("emi" = "N2O", "accounts" = c("Field burning of agricultural residues")),
    # or full Waste management n2O?
    "n2owaste" = list("emi" = "N2O", "accounts" = c("Wastewater treatment and discharge")),
    "co2cement_process" = list("emi" = "CO2", "accounts" = c("Industrial processes and product use")),
    "co2luc" = list("emi" = "CO2", "accounts" = c("Land use, land use change, and forestry (LULUCF)"))
  )

  # other MACCs do not have a direct mapping to the eurostat data (use sector information instead)
  # ch4gas + ch4oil -> extraction.process.ch4 - ch4coal #nolint
  # ch4forest + ch4savan -> lulucf.process.ch4 #nolint
  # n2ofertin + n2ofertcr + n2ofertsom + n2oanwstc + n2oanwstm + n2oanwstp -> agriculture.process.n2o - n2oagwaste #nolint
  # n2oforest + n2osavan -> lulucf.process.n2o #nolint
  # co2cement + co2chemicals + co2steel (=co2cement_process?) -> indst.process.co2

  x <- mbind(lapply(names(mapping), function(var) {
    setNames(dimSums(x[, , mapping[[var]]$accounts][, , mapping[[var]]$emi], na.rm = TRUE, dim = 3.2), var)
  }))
}
