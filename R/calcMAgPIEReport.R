#' @importFrom quitte inline.data.frame

calcMAgPIEReport <- function(subtype){

  x <- readSource("MAgPIE", subtype = "MAgPIEReport_extensive")

  if (subtype == "CostTotal") {

    x <- x[,,"Costs|MainSolve w/o GHG Emissions (million US$05/yr)"]/1000/1000 # with transformation factor from 10E6 US$2005 to 10E12 US$2005
    d <- "Total Landuse Costs from MAgPIE excluding emission costs"
    u <- "T$2005/yr"

  } else if (subtype == "CostMAC") {

    x <- x[,,"Costs|MainSolve|MACCS (million US$05/yr)"]/1000/1000 # with transformation factor from 10E6 US$2005 to 10E12 US$2005
    d <- "MAC Costs for LU emissions from MAgPIE"
    u <- "T$2005/yr"

  } else if (subtype == "ProductionBiomass") {

    x <- x[,,"Demand|Bioenergy|++|2nd generation (EJ/yr)"]/31.536 # EJ to TWa
    d <- "Production of ligno-cellulosic purpose grown biomass in MAgPIE"
    u <- "TWa/yr"

  } else if (subtype == "ch4n2o") {

    mapping <- inline.data.frame(
      "oldnames;newnames",
      "Emissions before technical mitigation|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr);ch4anmlwst",
      "Emissions before technical mitigation|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr);ch4animals",
      "Emissions before technical mitigation|CH4|Land|Agriculture|+|Rice (Mt CH4/yr);ch4rice",
      "Emissions before technical mitigation|N2O|Land|Agriculture|+|Animal Waste Management (Mt N2O/yr);n2oanwstm",
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Decay of Crop Residues (Mt N2O/yr);n2ofertcr",
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Inorganic Fertilizers (Mt N2O/yr);n2ofertin",
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Manure applied to Croplands (Mt N2O/yr);n2oanwstc",
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Pasture (Mt N2O/yr);n2oanwstp",
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss (Mt N2O/yr);n2ofertsom")

    x <- x[,,mapping$oldnames]
    # rename
    getNames(x,dim=3) <- mapping$newnames
    d <- "CH4 and N2O land emissions"
    u <- "MtCH4/yr and Mt N2O/yr"

  } else if (subtype == "co2") {

    mapping <- inline.data.frame(
      "oldnames;newnames",
      "Emissions|CO2|Land (Mt CO2/yr);co2luc")

    x <- x[,,mapping$oldnames]
    # rename
    getNames(x,dim=3) <- mapping$newnames
    d <- "CO2 land emissions"
    u <- "Mt CO2/yr"

  } else {
    stop("Unknown subtype",subtype)
  }

  # remove model and variable name
  x <- collapseNames(x)

  # !!! ATTENTION !!!
  # If you change the name of the baseline scenario from "none" to something else update "none" in calcMacBaseLandUse.R

  # Rename the MAgPIE scenarios to RCP scenarios
  getNames(x) <- getNames(x) %>%
    stringr::str_replace_all(c(
      "^C_"               = "",
      "-PkBudg900-mag-4"  = ".rcp20", # in 2022-10 still in emulator files
      "-PkBudg500-mag-4"  = ".rcp20",
      "-PkBudg1300-mag-4" = ".rcp26", # in 2022-10 still in emulator files
      "-PkBudg1150-mag-4" = ".rcp26",
      "-NDC-mag-4"        = ".rcp45",
      "-Base-mag-4"       = ".none"
  ))

  return(list(x           = x,
              weight      = NULL,
              unit        = u,
              description = d))
}
