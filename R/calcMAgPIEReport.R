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
      "magpieNames;remindNames",
      "Emissions|CO2|Land RAW|+|Land-use Change (Mt CO2/yr);co2luc",
      "Emissions|CO2|Land RAW|Land-use Change|+|Deforestation (Mt CO2/yr);co2lucPos",
      "Emissions|CO2|Land RAW|Land-use Change|+|Forest degradation (Mt CO2/yr);co2lucPos",
      "Emissions|CO2|Land RAW|Land-use Change|+|Other land conversion (Mt CO2/yr);co2lucPos",
      "Emissions|CO2|Land RAW|Land-use Change|+|Wood Harvest (Mt CO2/yr);co2lucPos",
      "Emissions|CO2|Land RAW|Land-use Change|Peatland|+|Positive (Mt CO2/yr);co2lucPos",
      "Emissions|CO2|Land RAW|Land-use Change|Peatland|+|Negative (Mt CO2/yr);co2lucNegIntentPeat",
      "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|CO2-price AR (Mt CO2/yr);co2lucNegIntentAR",
      "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|NPI_NDC AR (Mt CO2/yr);co2lucNegIntentAR",
      "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|Cropland Tree Cover (Mt CO2/yr);co2lucNegIntentAgroforestry",
      "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|Other Land (Mt CO2/yr);co2lucNegUnintent",
      "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|Secondary Forest (Mt CO2/yr);co2lucNegUnintent",
      "Emissions|CO2|Land RAW|Land-use Change|Regrowth|+|Timber Plantations (Mt CO2/yr);co2lucNegUnintent",
      "Emissions|CO2|Land RAW|Land-use Change|Residual|+|Positive (Mt CO2/yr);co2lucPos",
      "Emissions|CO2|Land RAW|Land-use Change|Residual|+|Negative (Mt CO2/yr);co2lucNegUnintent",
      "Emissions|CO2|Land RAW|Land-use Change|Soil|++|Emissions (Mt CO2/yr);co2lucPos",
      "Emissions|CO2|Land RAW|Land-use Change|Soil|Cropland management|+|Withdrawals (Mt CO2/yr);co2lucNegUnintent",
      "Emissions|CO2|Land RAW|Land-use Change|Soil|Land Conversion|+|Withdrawals (Mt CO2/yr);co2lucNegUnintent",
      "Emissions|CO2|Land RAW|Land-use Change|Soil|Soil Carbon Management|+|Withdrawals (Mt CO2/yr);co2lucNegIntentSCM",
      "Emissions|CO2|Land RAW|Land-use Change|Timber|+|Storage in HWP (Mt CO2/yr);co2lucNegIntentTimber",
      "Emissions|CO2|Land RAW|Land-use Change|Timber|+|Release from HWP (Mt CO2/yr);co2lucPos"
    )

    # aggregate (sum over) MAgPIE variables to REMIND entys
    x <- toolAggregate(x[, , mapping$magpieNames], rel = mapping, from = "magpieNames", to = "remindNames", dim = 3.3)

    d <- "CO2 land emissions"
    u <- "Mt CO2/yr"

  } else if (subtype == "AirPollutants") {

    createMappingMag2Rem <- function(replacement) {

      # Define generic mapping from MAgPIE to REMIND variable names for all species
      # Emissions|X|Land|+|Agriculture  endogenous  NH3, NO2
      # Emissions|X|AFOLU|Agriculture   exogenous   BC, CO, OC, SO2 and VOC : all zero --> dont import them from MAgPIE report
      # Peatland emissions are zero and ignored completely
      #'Emissions|SPECIES|Land|+|Peatland (Mt SPECIES/yr)'                                 , 'Emi|SPECIES|AFOLU|Land|+|Peatland (Mt SPECIES/yr)'                ,

      mag2remGeneric <- tibble::tribble(
        ~mag                                                                                , ~rem                                                               ,
        'Emissions|SPECIES|Land|+|Agriculture (Mt SPECIES/yr)'                              , 'Emi|SPECIES|AFOLU|+|Agriculture (Mt SPECIES/yr)'                  ,
        'Emissions|SPECIES|Land|Biomass Burning|+|Burning of Crop Residues (Mt SPECIES/yr)' , 'Emi|SPECIES|AFOLU|+|Agricultural Waste Burning (Mt SPECIES/yr)'   ,
        'Emissions|SPECIES|AFOLU|Land|Fires (Mt SPECIES/yr)'                                , 'Emi|SPECIES|AFOLU|Land|+|Fires (Mt SPECIES/yr)'                   ,
        'Emissions|SPECIES|AFOLU|Land|Fires|+|Forest Burning (Mt SPECIES/yr)'               , 'Emi|SPECIES|AFOLU|Land|Fires|+|Forest Burning (Mt SPECIES/yr)'    ,
        'Emissions|SPECIES|AFOLU|Land|Fires|+|Grassland Burning (Mt SPECIES/yr)'            , 'Emi|SPECIES|AFOLU|Land|Fires|+|Grassland Burning (Mt SPECIES/yr)' ,
        'Emissions|SPECIES|AFOLU|Land|Fires|+|Peat Burning (Mt SPECIES/yr)'                 , 'Emi|SPECIES|AFOLU|Land|Fires|+|Peat Burning (Mt SPECIES/yr)'
      )

      # replace "SPECIES" with the actual species name given from sapply below
      mag2remGeneric |> mutate(across(dplyr::everything(), ~ gsub("SPECIES", replacement, .x)))

    }

    species <- c("BC","CO","NH3","NO2","OC","SO2","VOC")

    # create mapping for all species
    mag2rem <- species |>
      sapply(createMappingMag2Rem,
             simplify = FALSE,
             USE.NAMES = TRUE) |>
      bind_rows()

    # Find magpie variable names that exist in the report and select only those from the mapping
    magpieNamesExisting <- mag2rem$mag %in% getNames(x, dim = 3)

    # Select only the existing variable names from the mapping for MAgPIE and REMIND respectively
    magpieNamesToUse <- mag2rem$mag[magpieNamesExisting]
    remindNamesToUse <- mag2rem$rem[magpieNamesExisting]

    # Reduce the data to the variables that we actually need
    x <- x[, , magpieNamesToUse]

    # Rename the MAgPIE variables to the REMIND variable names
    getNames(x, dim = 3) <- remindNamesToUse

    # Rename NO2 into NOx
    getNames(x, dim = 3) <- gsub("NO2", "NOx", getNames(x, dim = 3))

    d <- "Air pollutant emissions from land"
    u <- "Mt/yr"

  } else if (subtype == "fertilizer") {
    mapping <- inline.data.frame(
      "magpieNames;remindNames",
      "Resources|Nitrogen|Pasture Budget|Inputs|+|Fertilizer (Mt Nr/yr);fertilizer",
      "Resources|Nitrogen|Cropland Budget|Inputs|+|Fertilizer (Mt Nr/yr);fertilizer"
    )

    # aggregate (sum over) MAgPIE variables to REMIND entys
    x <- toolAggregate(x[, , mapping$magpieNames], rel = mapping, from = "magpieNames", to = "remindNames", dim = 3.3)

    d <- "Fertilizer Input"
    u <- "Mt Nr/yr"

  } else {
    stop("Unknown subtype", subtype)
  }

  # remove model and variable name
  x <- collapseNames(x)

  # !!! ATTENTION !!!
  # If you change the name of the baseline scenario from "none" to something else update "none" in calcMacBaseLandUse.R

  datetimepattern <- "_20[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}"

  # Rename the MAgPIE scenarios: e.g. "C_SSP1-PkBudg1000_2026-05-08_01.33.54-mag-4" to "SSP1.rcp26"
  getNames(x, dim = "scenario") <- getNames(x, dim = "scenario") %>%
      stringr::str_replace_all(datetimepattern, "") %>%
      stringr::str_replace_all(c(
      "^C_"               = "",
      "-PkBudg750-mag-4"  = ".rcp20",
      "-PkBudg1000-mag-4" = ".rcp26",
      "-NPi2025-mag-4"    = ".rcp45",
      #"-rollBack-mag-4"   = ".rcp45", # only SSP3 available
      "-NDC-mag-4"        = ".rcp37"
      # "-Base-mag-4"       = ".none",  # nolint
    ))

  # Fill missing scenarios expected in REMIND with scenarios that are closest to them and rename them to the expected scenario names in REMIND
  # no SSP5 data available --> use SSP2 data
  SSP5 <- x[, , "SSP2"]
  getNames(SSP5, dim = 1) <- "SSP5"
  x <- mbind(x, SSP5)
  # rcp37 must be added to the scenario_config.csv for SSP2-NDC that currently takes 4.5 from the NPI2025


  return(list(
    x = x,
    weight = NULL,
    unit = u,
    description = d
  ))
}
