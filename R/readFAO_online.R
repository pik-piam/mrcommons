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
#' }
#' @return FAO data as MAgPIE object
#' @author Ulrich Kreidenweis, Abhijeet Mishra, Mishko Stevanovic, David Klein, Edna Molina Bacca
#' @seealso [readSource()]
#' @examples
#'
#'   \dontrun{ a <- readSource("FAO_online","Crop")
#'   }
#' @importFrom tools file_ext
#' @importFrom data.table fread
#' @importFrom utils unzip
#' @importFrom tools file_path_sans_ext
#' @importFrom tidyr pivot_longer starts_with

readFAO_online <- function(subtype) {

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
    FbsHistoric             = c("FoodBalanceSheetsHistoric_E_All_Data.zip" ),
    Fbs                     = c("FoodBalanceSheets_E_All_Data_(Normalized).zip"),#old and new FBS
    Fertilizer              = c("Environment_Fertilizers_E_All_Data.zip"),# should not be used, use CB and FS or calcFAOharmonized() instead
    FertilizerNutrients     = c("Inputs_FertilizersNutrient_E_All_Data_(Normalized).zip"),
    FertilizerProducts      = c("Inputs_FertilizersProduct_E_All_Data_(Normalized).zip"),
    Fodder                  = c("Fodder.csv"),
    FoodSecurity            = c("Food_Security_Data_E_All_Data.zip"),
    ForestProdTrade         = c("Forestry_E_All_Data_(Normalized).zip"),
    Land                    = c("Resources_Land_E_All_Data.zip","Inputs_LandUse_E_All_Data_(Normalized).zip"), # old source file: Resources_Land_E_All_Data.zip
    LiveHead                = c("Production_Livestock_E_All_Data.zip"),
    LivePrim                = c("Production_LivestockPrimary_E_All_Data.zip"),
    LiveProc                = c("Production_LivestockProcessed_E_All_Data.zip"),
    Pop                     = c("Population_E_All_Data.zip"),
    PricesProducerAnnual    = c("Prices_E_All_Data.zip"),
    PricesProducerAnnualLCU = c("Prices_E_All_Data.zip"),
    ValueOfProd             = c("Value_of_Production_E_All_Data.zip")
    )


  file <- toolSubtypeSelect(subtype,files)

  # ---- Read the first file you find, prefer normalized format ----

  try_files <- NULL

  # Add an entry with "Normalized" in front of the current entry in the file list if the current entry does not contain "Normalized".
  for (fi in file) {
    extension <- file_ext(basename(fi))
    if (grepl("Normalized",fi)) {
      try_files <- c(try_files,fi)
    } else {

      try_files <- c(paste0(file_path_sans_ext(fi),"_(Normalized).",extension),fi)
    }
  }

  # look for data in normalized (i.e. long) format first before looking for the wide format
  # decompress if it is zipped
  for(file in try_files) {
    extension <- file_ext(basename(file))
    csv_name <- paste0(file_path_sans_ext(file), ".csv")
    if (file.exists(csv_name)) {
      file <- csv_name
      break
    } else if(extension=="zip" & file.exists(file)){
      files_extracted <- unzip(file, exdir = tempdir()) # use the absolute path to the file in order to unzip when working in the function
      file <- paste0(tempdir(), "/",csv_name)
      on.exit(file.remove(files_extracted))
      break
    }
  }

  # ---- Select columns to be read from file and read file ----

  ## efficient reading of csv file: read only needed columns in the needed type (codes as factor)
  csvcolnames <- colnames(read.table(file, header=T, nrows=1, sep=","))

  # check if data is in long or wide format
  long <- ifelse("Year" %in% csvcolnames,TRUE,FALSE)

  # define vector with types corresponding to the columns in the file
  readcolClass <- rep("NULL",length(csvcolnames))
  readcolClass[csvcolnames %in% c("Area.Code","Country.Code","CountryCode","Item.Code","ItemCode","Element.Code","ElementCode")] <- "factor"
  readcolClass[csvcolnames %in% c("Area","Country","Element","Item","Unit","Months")] <- "character"
  readcolClass[csvcolnames %in% c("Value","Year")] <- NA
  if (!long) readcolClass[grepl("Y[0-9]{4}$",csvcolnames)] <- NA

  FAO <- fread(input=file, header=F, skip=1, sep=",", colClasses=readcolClass, col.names= csvcolnames[is.na(readcolClass) | readcolClass != "NULL"], quote = "\"", encoding = "Latin-1", showProgress = FALSE)
  FAO <- as.data.frame(FAO)
  # from wide to long (move years from individual columns into one column)
  if (!long) FAO <- pivot_longer(FAO,cols = starts_with("Y"),names_to = "Year", names_pattern = "Y(.*)", names_transform = list("Year" = as.integer), values_to = "Value")
  # subtype 'PricesProducerAnnual' contains annual and seasonal data. Select annual data only and delete 'Months' column afterwards
  if ("Months" %in% names(FAO)) {
    FAO <- FAO[FAO$Months == "Annual value",]
    FAO <- FAO[,!names(FAO) %in% "Months"]
  }

  names(FAO)[names(FAO) == "Area.Code"] <- "CountryCode"
  names(FAO)[names(FAO) == "Area"] <- "Country"
  names(FAO) <- gsub("\\.","",names(FAO))

  # ---- Assigning the ISO codes to countries ----

  # Load FAO specific countries (not included in country2iso.csv in madrat)
  FAOiso_faocode <- toolGetMapping("FAOiso_faocode_online.csv", where="mrcommons")
  # convert data frame into named vector as required by toolCountry2isocode
  FAOiso_faocode <- structure(as.character(FAOiso_faocode$ISO),names=as.character(FAOiso_faocode$Country))
  # look up ISO codes using central definition and extra FAO mapping from line above
  FAO$ISO <- toolCountry2isocode(FAO$Country,mapping = FAOiso_faocode)
  # remove country aggregates (CountryCode >= 5000, formerly had '(Total)' in their name)
  FAO <- FAO[as.integer(levels(FAO$CountryCode)[FAO$CountryCode])<5000,]
  # remove countries with missing ISO code
  FAO <- FAO[!is.na(FAO$ISO),]

  # ---- Convert units ----

  # define helper function for unit conversion
  .convert.unit <- function(x, old_unit, new_unit, factor) {
    replace <- x$Unit == old_unit
    if(any(replace)){
      x$Value[replace] <- x$Value[replace]*factor
      x$Unit[replace] <- new_unit
    }
    return(x)
  }

  ### convert some units
  FAO <- .convert.unit(x = FAO, old_unit = "1000 tonnes", new_unit = "tonnes", factor = 1000)
  FAO <- .convert.unit(x = FAO, old_unit = "1000 Head",   new_unit = "Head",   factor = 1000)
  FAO <- .convert.unit(x = FAO, old_unit = "1000 number", new_unit = "number", factor = 1000)
  FAO <- .convert.unit(x = FAO, old_unit = "1000",        new_unit = "number", factor = 1000)
  FAO <- .convert.unit(x = FAO, old_unit = "1000 Ha",     new_unit = "ha",     factor = 1000)
  FAO <- .convert.unit(x = FAO, old_unit = "1000 persons",new_unit = "persons",factor = 1000)

  # ---- Reformat elements ----

  elementShort <- toolGetMapping("FAOelementShort.csv", where="mrcommons")
  # keep relevant rows only
  elementShort <- elementShort[elementShort$ElementCode %in% FAO$ElementCode,]

  # make ElementShort a combination of Element and Unit, replace special characters, and replace multiple _ by one
  tmp_element <- gsub("[\\.,;?\\+& \\/\\-]","_",FAO$Element, perl=TRUE)
  tmp_unit    <- gsub("[\\.,;\\+& \\-]","_",    FAO$Unit, perl=TRUE)
  tmp_elementshort <- paste0(tmp_element,"_(",tmp_unit,")")
  FAO$ElementShort <- gsub("_{1,}","_", tmp_elementshort, perl = TRUE)

  ### replace ElementShort with the entries from ElementShort if the Unit is the same
  if (length(elementShort) > 0) {
    for (i in 1:nrow(elementShort)) {
      FAO$ElementShort[FAO$ElementCode == elementShort[i,"ElementCode"] & FAO$Unit == elementShort[i,"Unit"]] <- as.character(elementShort[i,"ElementShort"])
    }
  }

  # remove accent in Mate to avoid problems and remove other strange names
  FAO$Item <- gsub("\u00E9","e",FAO$Item, perl=TRUE)
  FAO$Item <- gsub("\n + (Total)", " + (Total)", FAO$Item, fixed = TRUE)
  FAO$ItemCodeItem <- paste0(FAO$ItemCode,"|", gsub("\\.","",FAO$Item,perl=TRUE))

  FAO <- as.magpie(FAO[,c("Year","ISO","ItemCodeItem","ElementShort","Value")], temporal=1, spatial=2, datacol=5)
  if(subtype %in% c("EmisAgBurnCropResid","EmisAgCropResid","EmisLuForest")) getNames(FAO, dim=1) <- gsub("\\r", "", getNames(FAO, dim=1))
  if(subtype %in% c("CapitalStock")) getNames(FAO) <- gsub("[\\%]", "percentage", getNames(FAO))
  if(subtype %in% c("CapitalStock")) getNames(FAO) <- gsub("[\\$]", "D", getNames(FAO))

  gc()

  FAO <- magpiesort(FAO)

  return(FAO)
}
