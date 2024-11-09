#' calcMacBase
#' calculate MacBase
#'
#' @param subtype Source of subset of emissions
#' @return magpie object
#' @author David Klein, Julian Oeser
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "MacBaseLandUse")
#' }
#'
#' @importFrom magclass dimSums setCells


calcMacBaseLandUse <- function(subtype) {
  # Create empty magclass object with all dimensions that can be filled below (so it's
  # easy to see which entries remain empty afterwards)
  isoCountry <- read.csv2(system.file("extdata", "iso_country.csv", package = "madrat"), row.names = NULL)
  sources    <- c("co2luc", "n2oanwstm", "n2ofertin", "n2oanwstc", "n2ofertcr", "n2ofertsom", "n2ofertrb", "n2oanwstp",
                  "n2opeatland", "n2oforest", "n2osavan", "n2oagwaste", "ch4rice", "ch4anmlwst", "ch4animals",
                  "ch4peatland", "ch4forest", "ch4savan", "ch4agwaste")

  y <- new.magpie(cells_and_regions = isoCountry$x,
                  years = seq(2005, 2150, 5),
                  names = sources,
                  sets = c("region", "year", "type"))
  y <- add_dimension(y,
                     dim = 3.2,
                     add = "c_LU_emi_scen",
                     nm = c("SSP1", "SSP2", "SSP3", "SSP5", "SDP", "SDP_EI", "SDP_RC", "SDP_MC", "SSP2EU","SSP2_lowEn"))
  y <- add_dimension(y,
                     dim = 3.3,
                     add = "rcp",
                     nm = c("rcp20", "rcp26", "rcp45", "none"))

  if (subtype == "MAgPIE") {
    # Read emission baselines for MAC in REMIND. These data have been calcualted by external scripts, that calcualte
    # the CO2 LUC MAC from a bunch of MAgpIE runs started only for the purpose to calculate the MAC.
    x <- readSource("MAgPIE", subtype = "macBase")

    getNames(x) <- gsub("\\.", "", getNames(x))
    # split up the fourth dimension again
    getNames(x) <- gsub("SSP", "\\.SSP", getNames(x))
    # make SDP scenario using SSP1 data
    xSDP <- x[, , "SSP1"]
    for (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
      getNames(xSDP) <- gsub("SSP1", i, getNames(x[, , "SSP1"]))
      x <- mbind(x, xSDP)
    }
    # make SSP2EU scenario using SSP2 data
    xSSP2EU <- x[, , "SSP2"]
    getNames(xSSP2EU) <- gsub("SSP2", "SSP2EU", getNames(xSSP2EU))
    x <- mbind(x, xSSP2EU)
    # Add missing rcp dimension (data only exists for Baseline=none, use Baseline data for RCPs)
    x <- add_dimension(x, dim = 3.3, add = "rcp", nm = c("rcp20", "rcp26", "rcp45", "none"))
    getSets(x) <- c("region", "year", "type", "c_LU_emi_scen", "rcp")

    # emission types that are updated with new MAgPIE 4 data
    emiMag <- c("co2luc",
                "n2oanwstm",
                "n2ofertin",
                "n2oanwstc",
                "n2ofertcr",
                "n2ofertsom",
                "n2oanwstp",
                "ch4rice",
                "ch4anmlwst",
                "ch4animals")

    x <- x[, , emiMag]

    # Replace CO2 LUC baseline for SSP2, since there is newer data from MAgPIE 4.0
    # (the tax000,0 scenario from the CO2MAC-2018 runs)
    xCO2 <- readSource("MAgPIE", subtype = "macBaseCO2luc")
    xCO2 <- time_interpolate(xCO2, getYears(x))

    # write co2 baseline to all RCPs
    x[, , "co2luc.SSP2"] <- xCO2

    # add values for SSP3 copying the values from SSP2
    tmp <- x[,,"SSP2"]
    getNames(tmp) <- gsub("SSP2", "SSP3", getNames(tmp))
    x <- mbind(x, tmp)

    # add values for SSP2_lowEn copying the values from SSP2
    tmp <- x[,,"SSP2"]
    getNames(tmp) <- gsub("SSP2", "SSP2_lowEn", getNames(tmp))
    x <- mbind(x, tmp)

    # Replace CH4 and N2O LUC baseline for SSP2 and SSP1, since there is newer data from a
    # coupled REMIND-MAgPIE 4.0 Baseline run
    # Read ch4 and n2o emissions from MAgpIE Base scenario
    xCH4N2O <- calcOutput("MAgPIEReport", subtype = "ch4n2o", aggregate = FALSE)
    # remove peatland emissions
    xCH4N2O <- xCH4N2O[, , c("ch4peatland", "n2opeatland"), invert = TRUE]
    # change the order of the entries in the 3. dimension
    getNames(xCH4N2O) <- sub("^([^\\.]*)\\.([^\\.]*)\\.([^\\.]*)$", "\\3.\\1.\\2", getNames(xCH4N2O))

    # convert Mt N2O/yr -> Mt N/yr
    n2oEntities <- getNames(xCH4N2O[, , "n2o", pmatch = TRUE])
    xCH4N2O[, , n2oEntities] <- xCH4N2O[, , n2oEntities] * 28 / 44

    xCH4N2O <- time_interpolate(xCH4N2O, getYears(x))
    getSets(xCH4N2O) <- c("region", "year", "type", "c_LU_emi_scen", "rcp") # use same names as x

    x[, , getNames(xCH4N2O)] <- xCH4N2O

    # FS: Australia-specific, adjust 2005-2015 MacBase for lucco2 to NGGI data for LULUCF CO2
    # to catch recent trend from afforestation/forest management:
    # http://ageis.climatechange.gov.au/,
    # graph: http://ageis.climatechange.gov.au/Chart_KP.aspx?OD_ID=79041341749&TypeID=2
    # does not agree with EDGAR data which shows even increasing trend,
    # yet I trust NGGI more also because of this article:
    # https://www.researchgate.net/publication/301942515_Deforestation_in_Australia_Drivers_trends_and_policy_responses
    # long-term solution for new regions not in Magpie still needed

    gtC2MtCO2 <- 3666.667

    xOldCO2luc <- x[, c("y2005", "y2010", "y2015"), "co2luc"]

    # adjust Australia historic co2luc emissions
    x["AUS", c("y2005"), "co2luc"] <- 70 / gtC2MtCO2
    x["AUS", c("y2010"), "co2luc"] <- 5 / gtC2MtCO2
    x["AUS", c("y2015"), "co2luc"] <- -20 / gtC2MtCO2

    # adjust Canada historic co2luc emissions s.t. CAZ has same emissions than before (from MagPIE runs)
    years <- c("y2005", "y2010", "y2015")
    x["CAN", years, "co2luc"] <- setCells(dimSums(xOldCO2luc[c("AUS", "CAN"), , ], dim = 1)
                                          - x["AUS", years, "co2luc"], "CAN")


  } else if (subtype == "DirectlyFromMAgPIE") {
    # Read emission baselines for REMIND directly from MAgPIE reports.
    # The reports are taken from coupled runs, not from runs that have only been started to calculate the MAC.

    # CO2: NO MAC was calculated, no MAC must be active in REMIND.
    # CH4/N2O: take emissions before technical mitigation from MAgPIE, apply MAC in REMIND (the same MAC as in MAgPIE).

    # emission types in REMIND that are updated with MAgPIE data
    emiMacMagpie <- c("co2luc",
                      "n2oanwstm",
                      "n2ofertin",
                      "n2oanwstc",
                      "n2ofertcr",
                      "n2ofertsom",
                      "n2oanwstp",
                      "n2opeatland",
                      "ch4rice",
                      "ch4anmlwst",
                      "ch4animals",
                      "ch4peatland")

    y <- y[, , emiMacMagpie]

    # Read CO2 LUC baseline for all SSPs/SDP from MAgPIE reports
    xCO2 <- calcOutput("MAgPIEReport", subtype = "co2", aggregate = FALSE, warnNA = FALSE)
    xCO2[, 1995, ] <- 0 # replace NA with 0 (only CO2 has NA in 1995)
    xCO2 <- add_dimension(xCO2, dim = 3.3, nm = "co2luc")
    getSets(xCO2) <- c("region", "year", "scenario", "variable", "data")

    # Read N2O, CH4 baseline for all SSPs/SDP from MAgPIE reports
    xCH4N2O <- calcOutput("MAgPIEReport", subtype = "ch4n2o", aggregate = FALSE)

    x <- mbind(xCO2, xCH4N2O)
    x <- time_interpolate(x, getYears(y))

    # change the order of the entries in the 3. dimension
    getNames(x) <- sub("^([^\\.]*)\\.([^\\.]*)\\.([^\\.]*)$", "\\3.\\1.\\2", getNames(x))

    # convert Mt N2O/yr -> Mt N/yr
    n2oEntities <- getNames(x[, , "n2o", pmatch = TRUE])
    x[, , n2oEntities] <- x[, , n2oEntities] * 28 / 44

    # convert Mt CO2/yr -> Gt C/yr
    co2Entities <- getNames(x[, , "co2", pmatch = TRUE])
    x[, , co2Entities] <- x[, , co2Entities] * 1 / 1000 * 12 / 44

    getSets(x) <- c("region", "year", "type", "c_LU_emi_scen", "rcp") # use same names as y

    # write co2,ch4,n2o baseline to all RCPs
    y[, , getNames(x)] <- x


  } else if (subtype == "Exogenous") {

    x <- readSource("MAgPIE", subtype = "macBase")

    getNames(x) <- gsub("\\.", "", getNames(x))
    # split up the fourth dimension again
    getNames(x) <- gsub("SSP", "\\.SSP", getNames(x))
    # make SDP scenario using SSP1 data
    xSDP <- x[, , "SSP1"]
    for (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
      getNames(xSDP) <- gsub("SSP1", i, getNames(x[, , "SSP1"]))
      x <- mbind(x, xSDP)
    }
    # make SSP2riadne scenario using SSP2 data
    xSSP2EU <- x[, , "SSP2"]
    getNames(xSSP2EU) <- gsub("SSP2", "SSP2EU", getNames(xSSP2EU))
    x <- mbind(x, xSSP2EU)
    # Add missing rcp dimension (data only exists for Baseline=none, use Baseline data for RCPs)
    x <- add_dimension(x, dim = 3.3, add = "rcp", nm = c("rcp20", "rcp26", "rcp45", "none"))
    getSets(x) <- c("region", "year", "type", "c_LU_emi_scen", "rcp")

    # emission subtype that are not updated with new MAgPIE data
    emiExo <- c("n2oforest",
                "n2osavan",
                "n2oagwaste",
                "ch4forest",
                "ch4savan",
                "ch4agwaste")

    # select Baseline (all other RCPs are only copies of Baseline anyway, see above)
    x <- collapseNames(x[, , emiExo][, , "none"], collapsedim = 3.3)

    # add values for SSP3 copying the values from SSP2
    tmp <- x[,,"SSP2"]
    getNames(tmp) <- gsub("SSP2", "SSP3", getNames(tmp))
    x <- mbind(x, tmp)

    # add values for SSP2_lowEn copying the values from SSP2
    tmp <- x[,,"SSP2"]
    getNames(tmp) <- gsub("SSP2", "SSP2_lowEn", getNames(tmp))
    x <- mbind(x, tmp)

  } else {
    stop("Unkown subtype: ", subtype)
  }

  return(list(x           = x,
              weight      = NULL,
              unit        = "unit",
              description = "baseline emissions of N2O and CH4 from landuse based on data from Magpie"))

}
