#' Convert subtypes of the PBL_MACC_2019 data
#'
#' Convert subtypes from PBL_MACC_2019 to data on ISO country level.#'
#'
#' @param x MAgPIE object containing PBL_MACC_2019 data on region level
#' @param subtype data subtype.
#' "ch4coal","ch4oil","ch4gas","ch4wstl","ch4wsts","ch4rice","ch4animals","ch4anmlwst",
#' "n2otrans","n2oadac","n2onitac","n2ofert","n2oanwst","n2owaste",
#' "HFC_tot", "SF6_tot", "PFC_tot" or "baseline_sources"
#' @return PBL_MACC_2019 data as MAgPIE object for all subtypes aggregated to
#' country level
#' @author Florian Humpenoeder
#' @seealso [madrat::readSource()]

convertPBL_MACC_2019 <- function(x, subtype) { # nolint : object_name_linter.

  map <- toolGetMapping(type = "regional", name = "regionmapping_IMAGE_PBL_MACC_2019.csv", where = "mappingfolder")

  if (subtype == "baseline_sources") {
    # convert back to CH4 and N2O using AR4 GWP factors
    x[, , "ch4coal"] <- x[, , "ch4coal"] / 25
    x[, , "ch4oil"] <- x[, , "ch4oil"] / 25
    x[, , "ch4gas"] <- x[, , "ch4gas"] / 25
    x[, , "ch4wstl"] <- x[, , "ch4wstl"] / 25
    x[, , "ch4wsts"] <- x[, , "ch4wsts"] / 25
    x[, , "ch4rice"] <- x[, , "ch4rice"] / 25
    x[, , "ch4animals"] <- x[, , "ch4animals"] / 25
    x[, , "ch4anmlwst"] <- x[, , "ch4anmlwst"] / 25
    x[, , "n2otrans"] <- x[, , "n2otrans"] / 298
    x[, , "n2oadac"] <- x[, , "n2oadac"] / 298
    x[, , "n2onitac"] <- x[, , "n2onitac"] / 298
    x[, , "n2ofert"] <- x[, , "n2ofert"] / 298
    x[, , "n2oanwst"] <- x[, , "n2oanwst"] / 298
    x[, , "n2owaste"] <- x[, , "n2owaste"] / 298
    # weight
    CEDS_CH4 <- readSource("CEDS", subtype = "CH4")[, 2015, ] # nolint : object_name_linter.
    macBaseLandUse <- calcOutput("MacBaseLandUse", subtype = "MAgPIE", aggregate = FALSE)[, 2015, ]
    emiMac <- calcOutput("EmiMac", aggregate = FALSE)
    w <- new.magpie(cells_and_regions = map$CountryCode, years = NULL, names = getNames(x), sets = names(dimnames(x)))
    w[, , "ch4coal"]        <- CEDS_CH4[, , "1B1_Fugitive-solid-fuels"]
    w[, , "ch4oil"]         <- CEDS_CH4[, , "1B2_Fugitive-petr-and-gas"]
    w[, , "ch4gas"]         <- CEDS_CH4[, , "1B2_Fugitive-petr-and-gas"]
    w[, , "ch4wstl"]        <- emiMac[, , "ch4wstl"]
    w[, , "ch4wsts"]        <- emiMac[, , "ch4wsts"]
    w[, , "ch4rice"]        <- dimReduce(macBaseLandUse[, , "ch4rice.SSP2.rcp26"])
    w[, , "ch4animals"]     <- dimReduce(macBaseLandUse[, , "ch4animals.SSP2.rcp26"])
    w[, , "ch4anmlwst"]     <- dimReduce(macBaseLandUse[, , "ch4anmlwst.SSP2.rcp26"])
    w[, , "n2otrans"]       <- emiMac[, , "n2otrans"]
    w[, , "n2oadac"]        <- emiMac[, , "n2oacid"]
    w[, , "n2onitac"]       <- emiMac[, , "n2oacid"] # correct?
    w[, , "n2ofert"]        <- dimSums(macBaseLandUse[, , c("n2ofertin.SSP2.rcp26", "n2ofertcr.SSP2.rcp26",
                                                            "n2ofertsom.SSP2.rcp26")])
    w[, , "n2oanwst"]       <- dimSums(macBaseLandUse[, , c("n2oanwstc.SSP2.rcp26", "n2oanwstm.SSP2.rcp26",
                                                            "n2oanwstp.SSP2.rcp26")])
    w[, , "n2owaste"]       <- emiMac[, , "n2owaste"]

    y <- toolAggregate(x, map, from = "RegionCode", to = "CountryCode", weight = w, dim = 1, wdim = 1)
    y <- toolCountryFill(y, fill = 0)

  } else {
    y <- toolAggregate(x, map, from = "RegionCode", to = "CountryCode")
    y <- toolCountryFill(y, fill = 0)
  }

  return(y)
}
