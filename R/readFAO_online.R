#' Read FAO_online
#'
#' Read in FAO data that has been downloaded from the FAOSTAT website.
#' Files with exception of fodder.csv are aquired according to downloadFAO.
#'
#' Update 23-Jan-2017 - Added FAO Forestry production and trade data (Abhi)
#'
#'
#' @param subtype Type of FAO data that should be read. Available types are:
#' \itemize{
#' \item `CBCrop`: Commodity Balance Crop (CommodityBalances_Crops_E_All_Data.zip)
#' \item `CBLive`: Commoditiy Balance Livestock (CommodityBalances_LivestockFish_E_All_Data.zip)
#' \item `Crop`: Production Crops ("Production_Crops_E_All_Data.zip")
#' \item `CropProc`: Production Crops Processed ("Production_CropsProcessed_E_All_Data.zip")
#' \item `Fbs`: Food Balance Sheet ("FoodBalanceSheets_E_All_Data.zip")
#' \item `Fertilizer`: Fertilizer ("Resources_Fertilizers_E_All_Data.zip")
#' \item `FertilizerProducts`: Fertilizer by product ("Inputs_FertilizersProduct_E_All_Data_(Normalized).zip")
#' \item `FertilizerNutrients`: Fertilizer by nutrient ("Inputs_FertilizersNutrient_E_All_Data_(Normalized).zip")
#' \item `Fodder`: Fodder (data that has been manually downloaded from the FAOSTAT website as
#' seperate .xls files via a search for "forage" and "fodder" withing
#' Production-Crops. These datasets have been added together to a "Fodder.csv" file)
#' \item `FoodSecurity`: Food Security Data ("Food_Security_Data_E_All_Data.zip")
#' \item `FSCrop`: Food Supply Crops ("FoodSupply_Crops_E_All_Data.zip")
#' \item `FSLive`: Food Supply Livestock ("FoodSupply_LivestockFish_E_All_Data.zip")
#' \item `Land`: Land ("Resources_Land_E_All_Data.zip")
#' \item `LiveHead`: Production Live Animals ("Production_Livestock_E_All_Data.zip")
#' \item `LivePrim`: Production Livestock Primary ("Production_LivestockPrimary_E_All_Data.zip")
#' \item `LiveProc`: Production Livestock Processed ("Production_LivestockProcessed_E_All_Data.zip")
#' \item `Pop`: Population ("Population_E_All_Data.zip")
#' \item `ForestProdTrade`: Forestry Production and Trade ("Forestry_E_All_Data_(Normalized).zip")
#' \item `PricesProducerAnnual`: Producer Prices - Annual ("Prices_E_All_Data.zip")
#' \item `PricesProducerAnnualLCU`: Producer Prices - Annual in LCU ("Prices_E_All_Data.zip")
#' \item `ValueOfProd`: Value of Agricultural Production ("Value_of_Production_E_All_Data.zip")
#' \item `ValueShares`: Value shares by industry and primary factors
#' \item `Trade`: Trade quantities and values
#' }
#' @return FAO data as MAgPIE object
#' @author Ulrich Kreidenweis, Abhijeet Mishra, Mishko Stevanovic, David Klein, Edna Molina Bacca
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("FAO_online", "Crop")
#' }
#' @importFrom data.table fread
#' @importFrom dplyr summarise filter group_by ungroup %>%
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom utils unzip
#' @importFrom withr local_tempdir
readFAO_online <- function(subtype) { # nolint

  # ---- Define subtypes and corresponding files ----

  files <- list(
    CapitalStock            = c("Investment_CapitalStock_E_All_Data_(Normalized).zip"),
    CBCrop                  = c("CommodityBalances_Crops_E_All_Data.zip"),
    CBLive                  = c("CommodityBalances_LivestockFish_E_All_Data.zip"),
    Crop                    = c("Production_Crops_E_All_Data.zip"),
    CropProc                = c("Production_CropsProcessed_E_All_Data.zip"),
    EmisAgBurnCropResid     = c("Emissions_Agriculture_Burning_crop_residues_E_All_Data.zip"),
    EmisAgBurnSavanna       = c("Emissions_Agriculture_Burning_Savanna_E_All_Data.zip"),
    EmisAgCropResid         = c("Emissions_Agriculture_Crop_Residues_E_All_Data.zip"),
    EmisAgCultOrgSoil       = c("Emissions_Agriculture_Cultivated_Organic_Soils_E_All_Data.zip"),
    EmisAgEnergy            = c("Emissions_Agriculture_Energy_E_All_Data.zip"),
    EmisAgEntericFerment    = c("Emissions_Agriculture_Enteric_Fermentation_E_All_Data.zip"),
    EmisAgManureManag       = c("Emissions_Agriculture_Manure_Management_E_All_Data.zip"),
    EmisAgManurePasture     = c("Emissions_Agriculture_Manure_left_on_pasture_E_All_Data.zip"),
    EmisAgManureSoil        = c("Emissions_Agriculture_Manure_applied_to_soils_E_All_Data.zip"),
    EmisAgRiceCult          = c("Emissions_Agriculture_Rice_Cultivation_E_All_Data.zip"),
    EmisAgSynthFerti        = c("Emissions_Agriculture_Synthetic_Fertilizers_E_All_Data.zip"),
    EmisAgTotal             = c("Emissions_Agriculture_Agriculture_total_E_All_Data.zip"),
    EmisLuBurnBiomass       = c("Emissions_Land_Use_Burning_Biomass_E_All_Data.zip"),
    EmisLuCrop              = c("Emissions_Land_Use_Cropland_E_All_Data.zip"),
    EmisLuForest            = c("Emissions_Land_Use_Forest_Land_E_All_Data.zip"),
    EmisLuGrass             = c("Emissions_Land_Use_Grassland_E_All_Data.zip"),
    EmisLuTotal             = c("Emissions_Land_Use_Land_Use_Total_E_All_Data.zip"),
    FSCrop                  = c("FoodSupply_Crops_E_All_Data.zip"),
    FSLive                  = c("FoodSupply_LivestockFish_E_All_Data.zip"),
    FbsHistoric             = c("FoodBalanceSheetsHistoric_E_All_Data.zip"),
    Fbs                     = c("FoodBalanceSheets_E_All_Data_(Normalized).zip"), # old and new FBS
    # should not be used, use CB and FS or calcFAOharmonized() instead
    Fertilizer              = c("Environment_Fertilizers_E_All_Data.zip"),
    FertilizerNutrients     = c("Inputs_FertilizersNutrient_E_All_Data_(Normalized).zip"),
    FertilizerProducts      = c("Inputs_FertilizersProduct_E_All_Data_(Normalized).zip"),
    Fodder                  = c("Fodder.csv"),
    FoodSecurity            = c("Food_Security_Data_E_All_Data.zip"),
    ForestProdTrade         = c("Forestry_E_All_Data_(Normalized).zip"),
    # old source file: Resources_Land_E_All_Data.zip
    Land                    = c("Resources_Land_E_All_Data.zip", "Inputs_LandUse_E_All_Data_(Normalized).zip"),
    LiveHead                = c("Production_Livestock_E_All_Data.zip"),
    LivePrim                = c("Production_LivestockPrimary_E_All_Data.zip"),
    LiveProc                = c("Production_LivestockProcessed_E_All_Data.zip"),
    Pop                     = c("Population_E_All_Data.zip"),
    PricesProducerAnnual    = c("Prices_E_All_Data.zip"),
    PricesProducerAnnualLCU = c("Prices_E_All_Data.zip"),
    Trade                   = c("Trade_CropsLivestock_E_All_Data_(Normalized).zip"),
    ValueOfProd             = c("Value_of_Production_E_All_Data.zip"),
    ValueShares             = c("Value_shares_industry_primary_factors_E_All_Data_(Normalized).zip")
  )


  file <- toolSubtypeSelect(subtype, files)

  # ---- Read the first file you find, prefer normalized format ----

  tryFiles <- NULL

  # Add an entry with "Normalized" in front of the current entry in the file list
  # if the current entry does not contain "Normalized".
  for (fi in file) {
    extension <- file_ext(basename(fi))
    if (grepl("Normalized", fi)) {
      tryFiles <- c(tryFiles, fi)
    } else {

      tryFiles <- c(paste0(file_path_sans_ext(fi), "_(Normalized).", extension), fi)
    }
  }

  # look for data in normalized (i.e. long) format first before looking for the wide format
  # decompress if it is zipped
  for (file in tryFiles) {
    extension <- file_ext(basename(file))
    csvName <- paste0(file_path_sans_ext(file), ".csv")
    if (file.exists(csvName)) {
      file <- csvName
      break
    } else if (extension == "zip" && file.exists(file)) {
      tempfolder <- local_tempdir()
      unzip(file, exdir = tempfolder)
      file <- file.path(tempfolder, csvName)
      break
    }
  }

  # ---- Select columns to be read from file and read file ----

  ## efficient reading of csv file: read only needed columns in the needed type (codes as factor)
  csvcolnames <- colnames(read.table(file, header = TRUE, nrows = 1, sep = ","))

  # check if data is in long or wide format
  long <- ifelse("Year" %in% csvcolnames, TRUE, FALSE)

  # define vector with types corresponding to the columns in the file
  readcolClass <- rep("NULL", length(csvcolnames))
  factorCols <- c("Area.Code", "Country.Code", "CountryCode", "Food.Value.Code",
                  "Industry.Code", "Factor.Code", "Item.Code", "ItemCode",
                  "Element.Code", "ElementCode")
  readcolClass[csvcolnames %in% factorCols] <- "factor"
  readcolClass[csvcolnames %in% c("Area", "Country", "Factor", "Food.Value",
                                  "Industry", "Element", "Item", "Unit", "Months")] <- "character"
  readcolClass[csvcolnames %in% c("Value", "Year")] <- NA
  if (!long) readcolClass[grepl("Y[0-9]{4}$", csvcolnames)] <- NA

  fao <- fread(input = file, header = FALSE, skip = 1, sep = ",", colClasses = readcolClass,
               col.names = csvcolnames[is.na(readcolClass) | readcolClass != "NULL"], quote = "\"",
               encoding = "Latin-1", showProgress = FALSE)
  fao <- as.data.frame(fao)
  # from wide to long (move years from individual columns into one column)
  if (!long) {
    fao <- pivot_longer(fao, cols = starts_with("Y"), names_to = "Year", names_pattern = "Y(.*)",
                        names_transform = list("Year" = as.integer), values_to = "Value")
  }
  # subtype 'PricesProducerAnnual' contains annual and seasonal data. Select annual data only
  # and delete 'Months' column afterwards
  if ("Months" %in% names(fao)) {
    fao <- fao[fao$Months == "Annual value", ]
    fao <- fao[, !names(fao) %in% "Months"]
  }

  names(fao)[names(fao) == "Area.Code"] <- "CountryCode"
  names(fao)[names(fao) == "Area"] <- "Country"
  names(fao) <- gsub("\\.", "", names(fao))

  # ---- Assigning the ISO codes to countries ----

  # Load FAO specific countries (not included in country2iso.csv in madrat)
  faoIsoFaoCode <- toolGetMapping("FAOiso_faocode_online.csv", where = "mrcommons")
  # convert data frame into named vector as required by toolCountry2isocode
  faoIsoFaoCode <- structure(as.character(faoIsoFaoCode$ISO), names = as.character(faoIsoFaoCode$Country))
  # look up ISO codes using central definition and extra FAO mapping from line above
  # ignore warnings from FAO aggregate and other irrelevant regions
  ignoreRegions <- c("Africa", "Americas", "Asia", "Australia & New Zealand", "Caribbean",
                     "Central America", "Central Asia", "Eastern Africa", "Eastern Asia",
                     "Eastern Europe", "Europe", "European Union", "Land Locked Developing Countries",
                     "Least Developed Countries", "Low Income Food Deficit Countries",
                     "Melanesia", "Middle Africa", "Net Food Importing Developing Countries",
                     "Netherlands Antilles (former)", "Northern Africa", "Northern America",
                     "Northern Europe", "Oceania", "Small Island Developing States",
                     "South America", "South-Eastern Asia", "Southern Africa", "Southern Asia",
                     "Southern Europe", "Western Africa", "Western Asia", "Western Europe",
                     "World", "Australia and New Zealand", "European Union (27)", "Antarctic Region",
                     "European Union (28)", "South-eastern Asia", "East/South Asia and Pacific",
                     "Annex I countries", "Non-Annex I countries", "OECD", " Africa (excluding intra-trade) ",
                     "Americas (excluding intra-trade)", "Asia (excluding intra-trade)",
                     "Australia and New Zealand (excluding intra-trade)",
                     "Caribbean (excluding intra-trade)", "Central America (excluding intra-trade)",
                     "Central Asia (excluding intra-trade)", "China (excluding intra-trade)",
                     "Eastern Africa (excluding intra-trade)", "Eastern Asia (excluding intra-trade)",
                     "Eastern Europe (excluding intra-trade)", "Europe (excluding intra-trade)",
                     "European Union (12) (excluding intra-trade)", "European Union (15) (excluding intra-trade)",
                     "European Union (25) (excluding intra-trade)",
                     "European Union (27) (excluding Croatia) (excluding intra-trade)",
                     "European Union (27) (excluding intra-trade)",
                     "European Union (28) (excluding intra-trade)",
                     "Land Locked Developing Countries (excluding intra-trade)",
                     "Least Developed Countries (excluding intra-trade)",
                     "Africa (excluding intra-trade)", "Low Income Food Deficit Countries (excluding intra-trade)",
                     "Melanesia (excluding intra-trade)", "Micronesia (excluding intra-trade)",
                     "Middle Africa (excluding intra-trade)",
                     "Net Food Importing Developing Countries (excluding intra-trade)",
                     "Northern Africa (excluding intra-trade)", "Northern America (excluding intra-trade)",
                     "Northern Europe (excluding intra-trade)", "Oceania (excluding intra-trade)",
                     "Polynesia (excluding intra-trade)",
                     "Small Island Developing States (excluding intra-trade)",
                     "South-Eastern Asia (excluding intra-trade)",
                     "South America (excluding intra-trade)", "Southern Africa (excluding intra-trade)",
                     "Southern Asia (excluding intra-trade)",
                     "Southern Europe (excluding intra-trade)", "Western Africa (excluding intra-trade)",
                     "Western Asia (excluding intra-trade)",
                     "Western Europe (excluding intra-trade)")

  fao$ISO <- toolCountry2isocode(fao$Country, mapping = faoIsoFaoCode, ignoreCountries = ignoreRegions) # nolint
  # remove country aggregates (CountryCode >= 5000, formerly had '(Total)' in their name)
  fao <- fao[as.integer(levels(fao$CountryCode)[fao$CountryCode]) < 5000, ]
  # remove countries with missing ISO code
  fao <- fao[!is.na(fao$ISO), ]

  # ---- Convert units ----

  # define helper function for unit conversion
  .convertUnit <- function(x, oldUnit, newUnit, factor) {
    replace <- x$Unit == oldUnit
    if (any(replace)) {
      x$Value[replace] <- x$Value[replace] * factor
      x$Unit[replace] <- newUnit
    }
    return(x)
  }

  ### convert some units
  fao <- .convertUnit(x = fao, oldUnit = "1000 tonnes", newUnit = "tonnes", factor = 1000)
  fao <- .convertUnit(x = fao, oldUnit = "1000 Head",   newUnit = "Head",   factor = 1000)
  fao <- .convertUnit(x = fao, oldUnit = "1000 number", newUnit = "number", factor = 1000)
  fao <- .convertUnit(x = fao, oldUnit = "1000",        newUnit = "number", factor = 1000)
  fao <- .convertUnit(x = fao, oldUnit = "1000 Ha",     newUnit = "ha",     factor = 1000)
  fao <- .convertUnit(x = fao, oldUnit = "1000 persons", newUnit = "persons", factor = 1000)


  # ---- Reformat elements ----

  elementShort <- toolGetMapping("FAOelementShort.csv", where = "mrcommons")
  # keep relevant rows only
  elementShort <- elementShort[elementShort$ElementCode %in% fao$ElementCode, ]

  # make ElementShort a combination of Element and Unit, replace special characters, and replace multiple _ by one
  tmpElement <- gsub("[\\.,;?\\+& \\/\\-]", "_", fao$Element, perl = TRUE)
  tmpUnit    <- gsub("[\\.,;\\+& \\-]", "_",    fao$Unit, perl = TRUE)
  tmpElementShort <- paste0(tmpElement, "_(", tmpUnit, ")")
  fao$ElementShort <- gsub("_{1,}", "_", tmpElementShort, perl = TRUE) # nolint

  ### replace ElementShort with the entries from ElementShort if the Unit is the same
  if (length(elementShort) > 0) {
    for (i in seq_len(nrow(elementShort))) {
      j <- (fao$ElementCode == elementShort[i, "ElementCode"] & fao$Unit == elementShort[i, "Unit"])
      fao$ElementShort[j] <- as.character(elementShort[i, "ElementShort"])
    }
  }

  if ("Item" %in% colnames(fao)) {
    # remove accent in Mate to avoid problems and remove other strange names
    fao$Item <- gsub("\u00E9", "e", fao$Item, perl = TRUE)                                # nolint
    fao$Item <- gsub("\n + (Total)", " + (Total)", fao$Item, fixed = TRUE)                # nolint
    fao$ItemCodeItem <- paste0(fao$ItemCode, "|", gsub("\\.", "", fao$Item, perl = TRUE)) # nolint
  }

  # trade data has element codes 5608 5609 for "Import_Quantity_(Head)"
  # and codes 5908 5909 for "Export_Quantity_(Head)" for the "Other food" product,
  # despite all other characteristics being the same
  # this leads to duplicate rows when converting to magclass, sum these up first below
  if (subtype == "Trade") {
    tmp <- fao %>%
      filter(.data$ItemCodeItem == "1848|Other food") %>%
      group_by(.data$Year, .data$ISO, .data$ItemCodeItem, .data$ElementShort) %>%
      summarise("Value" = sum(.data$Value, na.rm = TRUE)) %>%
      ungroup()
    fao <-  fao[which(fao[, "ItemCodeItem"] != "1848|Other food"),
                c("Year", "ISO", "ItemCodeItem", "ElementShort", "Value")]
    fao <- rbind(tmp, fao)
  }

  # Value Shares has no items, but rather food values, industries, and factor dimensions
  if (subtype == "ValueShares") {
    fao$FoodValueCodeFoodValue <- paste0(fao$FoodValueCode, "|", gsub("\\.", "", fao$FoodValue, perl = TRUE)) # nolint
    fao$IndustryCodeIndustry <- paste0(fao$IndustryCode, "|", gsub("\\.", "", fao$Industry, perl = TRUE)) # nolint
    fao$FactorCodeFactor <- paste0(fao$FactorCode, "|", gsub("\\.", "", fao$Factor, perl = TRUE)) # nolint

    fao <- as.magpie(fao[, c("Year", "ISO", "FoodValueCodeFoodValue", "IndustryCodeIndustry",
                             "FactorCodeFactor", "ElementShort", "Value")],
                     temporal = 1, spatial = 2, datacol = 7)

  } else {

    fao <- as.magpie(fao[, c("Year", "ISO", "ItemCodeItem", "ElementShort", "Value")],
                     temporal = 1, spatial = 2, datacol = 5)

  }
  if (subtype %in% c("EmisAgBurnCropResid", "EmisAgCropResid", "EmisLuForest")) {
    getNames(fao, dim = 1) <- gsub("\\r", "", getNames(fao, dim = 1))
  }
  if (subtype %in% c("CapitalStock")) getNames(fao) <- gsub("[\\%]", "percentage", getNames(fao))
  if (subtype %in% c("CapitalStock")) getNames(fao) <- gsub("[\\$]", "D", getNames(fao))

  gc()

  fao <- magpiesort(fao)

  return(fao)
}
