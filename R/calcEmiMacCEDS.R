#' Calculate baseline emissions for maccs (mostly) from CEDS2024 data
#'
#' Provides REMIND data for baseline emissions for maccs from CEDS2024
#' Note that this can't fully replace the old EDGAR data, as it doesn't include
#' land use CO2 emissions. These are not currently actually used in REMIND
#' by default as the LUC CO2 MACC is off, but it's still not officially deprecated.
#'
#' Therefore, the CO2 from LUC is read from EDGAR with a 2005, a potentially
#' different baseyear than the one chosen in the parameter
#'
#' @param baseyear year to take as a reference from CEDS, ignored for the EDGAR2005 LUC CO2 emissions
#' @return REMIND data for baseline emissions for maccs and corresonding
#' weights (NULL) as a list of two MAgPIE objects
#' @author Gabriel Abrahao
#' @seealso [calcOutput()], [readSource()]
#' @importFrom magclass getNames<- getYears<-

calcEmiMacCEDS <- function(baseyear = 2020) {
  fullceds <- readSource("CEDS2024")[, baseyear, ]

  # emissions for the calculation of econometric paramter p1
  co2 <- fullceds[, , "co2_c"] * 1e-3 # MtC to GtC

  n2o <- fullceds[, , "n2o_n"] # Already in MtN
  ch4 <- fullceds[, , "ch4"] # Already in MtCH4

  co2cement <- dimSums(co2[, , c(
    "2A1_Cement-production",
    "2A2_Lime-production",
    "2Ax_Other-minerals"
  )], dim = 3)
  getNames(co2cement) <- "co2cement_process"
  ch4wsts <- dimSums(ch4[, , c("5D_Wastewater-handling")], dim = 3)
  getNames(ch4wsts) <- "ch4wsts"
  ch4wstl <- dimSums(ch4[, , c(
    "5A_Solid-waste-disposal", "5C_Waste-combustion", "5E_Other-waste-handling"
  )], dim = 3)
  getNames(ch4wstl) <- "ch4wstl"
  n2owaste <- dimSums(n2o[, , c(
    "5A_Solid-waste-disposal", "5C_Waste-combustion", "5D_Wastewater-handling", "5E_Other-waste-handling"
  )], dim = 3)
  getNames(n2owaste) <- "n2owaste"
  n2otrans <- dimSums(n2o[, , c(
    "1A3b_Road",
    "1A3c_Rail",
    "1A3di_Oil_Tanker_Loading",
    "1A3dii_Domestic-navigation",
    "1A3eii_Other-transp"
  )], dim = 3) # CEDS only reports 1A3aii_Domestic-aviation at the global level
  getNames(n2otrans) <- "n2otrans"
  n2oacid <- dimSums(n2o[, , c(
    "2B_Chemical-industry", "2B2_Chemicals-Nitric-acid", "2B3_Chemicals-Adipic-acid"
  )], dim = 3)
  getNames(n2oacid) <- "n2oacid"


  # Can't provide LUC CO2 from CEDS2024, so read EDGAR and fill it with 2005.
  edco2 <- readSource("EDGAR", subtype = "co2") * 12 / 44 * 1e-6
  co2luc <- dimSums(edco2[, , c("5A", "5D", "5F2")], dim = 3)
  getNames(co2luc) <- "co2luc"
  getYears(co2luc) <- baseyear

  # overwritting european countries with eurostat data if available
  euCountries <- c(
    "ALA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FRO",
    "FIN", "FRA", "DEU", "GIB", "GRC", "GGY", "HUN", "IRL", "IMN", "ITA", "JEY", "LVA", "LTU",
    "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR"
  )

  baselineEurostat <- calcOutput("HistEmissions", subtype = "MAC", aggregate = FALSE)
  if (baseyear %in% getYears(baselineEurostat)) {
    n2owaste[euCountries, baseyear, "n2owaste"] <- baselineEurostat[euCountries, baseyear, "n2owaste"]
    n2otrans[euCountries, baseyear, "n2otrans"] <- baselineEurostat[euCountries, baseyear, "n2otrans"]
    n2oacid[euCountries, baseyear, "n2oacid"] <- dimSums(
      baselineEurostat[euCountries, baseyear, c("n2oadac", "n2onitac")]
    )
    ch4wsts[euCountries, baseyear, "ch4wsts"] <- baselineEurostat[euCountries, baseyear, "ch4wsts"]
    ch4wstl[euCountries, baseyear, "ch4wstl"] <- baselineEurostat[euCountries, baseyear, "ch4wstl"]
  }

  # combine all parameters
  x <- mbind(co2cement, co2luc, n2owaste, n2otrans, n2oacid, ch4wsts, ch4wstl)
  getYears(x) <- NULL
  return(list(
    x = x,
    weight = NULL,
    unit = "GtC, MtCH4, MtN",
    description = paste0(
      "emissions in ", baseyear, " from CEDS, also 2005 LUC CO2 emissions from EDGAR"
    ),
    note = c("used to calculate econometric emission parameter p1")
  ))
}
