#' @importFrom quitte inline.data.frame

calcMAgPIEReport <- function(subtype) {
  x <- readSource("MAgPIE", subtype = "MAgPIEReport_extensive")

  if (subtype == "CostTotal") {
    # with transformation factor from 10E6 US$2017 to 10E12 US$2017
    x <- x[, , "Costs Without Incentives (million US$2017/yr)"] / 1000 / 1000
    d <- "Total Landuse Costs from MAgPIE excluding emission costs"
    u <- "trillion US$2017/yr"

  } else if (subtype == "CostMAC") {
    # with transformation factor from 10E6 US$2017 to 10E12 US$2017
    x <- x[, , "Costs Accounting|+|MACCS (million US$2017/yr)"] / 1000 / 1000
    d <- "MAC Costs for LU emissions from MAgPIE"
    u <- "trillion US$2017/yr"

  } else if (subtype == "ProductionBiomass") {
    x <- x[, , "Demand|Bioenergy|2nd generation|++|Bioenergy crops (EJ/yr)"] / 31.536 # EJ to TWa
    d <- "Production of ligno-cellulosic purpose grown biomass in MAgPIE"
    u <- "TWa/yr"

  } else if (subtype == "ch4n2o") {
    # nolint start
    mapping <- inline.data.frame(
      "oldnames;newnames",
      "Emissions before technical mitigation|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr);ch4anmlwst",
      "Emissions before technical mitigation|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr);ch4animals",
      "Emissions before technical mitigation|CH4|Land|Agriculture|+|Rice (Mt CH4/yr);ch4rice",
      "Emissions|CH4|Land|+|Peatland (Mt CH4/yr);ch4peatland",
      "Emissions before technical mitigation|N2O|Land|Agriculture|+|Animal Waste Management (Mt N2O/yr);n2oanwstm",
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Decay of Crop Residues (Mt N2O/yr);n2ofertcr",
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Inorganic Fertilizers (Mt N2O/yr);n2ofertin",
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Manure applied to Croplands (Mt N2O/yr);n2oanwstc",
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Pasture (Mt N2O/yr);n2oanwstp",
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss (Mt N2O/yr);n2ofertsom",
      "Emissions|N2O|Land|+|Peatland (Mt N2O/yr);n2opeatland"
    )
    # nolint end

    x <- x[, , mapping$oldnames]
    # rename
    getNames(x, dim = 3) <- mapping$newnames
    d <- "CH4 and N2O land emissions"
    u <- "MtCH4/yr and Mt N2O/yr"

  } else if (subtype == "co2") {
    mapping <- inline.data.frame(
      "oldnames;newnames",
      "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr);co2luc"
    )

    x <- x[, , mapping$oldnames]
    # rename
    getNames(x, dim = 3) <- mapping$newnames
    d <- "CO2 land emissions"
    u <- "Mt CO2/yr"

  } else {
    stop("Unknown subtype", subtype)
  }

  # remove model and variable name
  x <- collapseNames(x)

  # !!! ATTENTION !!!
  # If you change the name of the baseline scenario from "none" to something else update "none" in calcMacBaseLandUse.R

  # Rename the MAgPIE scenarios
  getNames(x) <- getNames(x) %>%
    stringr::str_replace_all(c(
      "^C_"               = "",
      # "-PkBudg900-mag-4"  = ".rcp20", # in 2022-10 still in emulator files
      # "-PkBudg500-mag-4"  = ".rcp20", # in 2023-10 still in emulator files
      "-PkBudg650-mag-4"    = ".rcp20",
      # "-PkBudg1300-mag-4" = ".rcp26", # in 2022-10 still in emulator files
      # "-PkBudg1150-mag-4" = ".rcp26", # in 2023-10 still in emulator files
      "-PkBudg1000-mag-4"   = ".rcp26",
      # "-NDC-mag-4"        = ".rcp45", # nolint
      "-NPi2025-mag-4"      = ".rcp45"
      # "-Base-mag-4"       = ".none",  # nolint
      # "SDP_MC"            = "SDP"     # nolint
    ))

  # add values for SSP2_lowEn-NPi2025 (infeasible) copying the values from SSP2-NPi2025
  tmp <- x[, , "SSP2.rcp45"]
  getNames(tmp) <- gsub("SSP2\\.rcp45", "SSP2_lowEn.rcp45", getNames(tmp))
  x <- mbind(x, tmp)

  return(list(
    x = x,
    weight = NULL,
    unit = u,
    description = d
  ))
}
