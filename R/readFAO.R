#' Read FAO
#'
#' Read in FAO data that has been bulk downloaded from the FAOSTAT website.
#' Files with exception of fodder.csv are aquired from:
#' http://faostat.fao.org/Portals/_Faostat/Downloads/zip_files/
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
#' @author Ulrich Kreidenweis, Abhijeet Mishra, Mishko Stevanovic
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("FAO", "Crop")
#' }
#' @importFrom data.table fread
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom utils unzip
#' @importFrom withr local_tempdir

readFAO <- function(subtype) {
  files <- c(
    CBCrop                  = "CommodityBalances_Crops_E_All_Data.zip",
    CBLive                  = "CommodityBalances_LivestockFish_E_All_Data.zip",
    Crop                    = "Production_Crops_E_All_Data.zip",
    CropProc                = "Production_CropsProcessed_E_All_Data.zip",
    EmisAgBurnCropResid     = "Emissions_Agriculture_Burning_crop_residues_E_All_Data.zip",
    EmisAgBurnSavanna       = "Emissions_Agriculture_Burning_Savanna_E_All_Data.zip",
    EmisAgCropResid         = "Emissions_Agriculture_Crop_Residues_E_All_Data.zip",
    EmisAgCultOrgSoil       = "Emissions_Agriculture_Cultivated_Organic_Soils_E_All_Data.zip",
    EmisAgEnergy            = "Emissions_Agriculture_Energy_E_All_Data.zip",
    EmisAgEntericFerment    = "Emissions_Agriculture_Enteric_Fermentation_E_All_Data.zip",
    EmisAgManureManag       = "Emissions_Agriculture_Manure_Management_E_All_Data.zip",
    EmisAgManurePasture     = "Emissions_Agriculture_Manure_left_on_pasture_E_All_Data.zip",
    EmisAgManureSoil        = "Emissions_Agriculture_Manure_applied_to_soils_E_All_Data.zip",
    EmisAgRiceCult          = "Emissions_Agriculture_Rice_Cultivation_E_All_Data.zip",
    EmisAgSynthFerti        = "Emissions_Agriculture_Synthetic_Fertilizers_E_All_Data.zip",
    EmisAgTotal             = "Emissions_Agriculture_Agriculture_total_E_All_Data.zip",
    EmisLuBurnBiomass       = "Emissions_Land_Use_Burning_Biomass_E_All_Data.zip",
    EmisLuCrop              = "Emissions_Land_Use_Cropland_E_All_Data.zip",
    EmisLuForest            = "Emissions_Land_Use_Forest_Land_E_All_Data.zip",
    EmisLuGrass             = "Emissions_Land_Use_Grassland_E_All_Data.zip",
    EmisLuTotal             = "Emissions_Land_Use_Land_Use_Total_E_All_Data.zip",
    FSCrop                  = "FoodSupply_Crops_E_All_Data.zip",
    FSLive                  = "FoodSupply_LivestockFish_E_All_Data.zip",
    # Fbs should not be used, use CB and FS or calcFAOharmonized() instead
    Fbs                     = "FoodBalanceSheets_E_All_Data.zip",
    Fertilizer              = "Environment_Fertilizers_E_All_Data.zip",
    Fodder                  = "Fodder.csv",
    FoodSecurity            = "Food_Security_Data_E_All_Data.zip",
    ForestProdTrade         = "Forestry_E_All_Data_(Normalized).zip",
    Land                    = "Resources_Land_E_All_Data.zip",
    LiveHead                = "Production_Livestock_E_All_Data.zip",
    LivePrim                = "Production_LivestockPrimary_E_All_Data.zip",
    LiveProc                = "Production_LivestockProcessed_E_All_Data.zip",
    Pop                     = "Population_E_All_Data.zip",
    PricesProducerAnnual    = "Prices_E_All_Data.zip",
    PricesProducerAnnualLCU = "Prices_E_All_Data.zip",
    ValueOfProd             = "Value_of_Production_E_All_Data.zip"
    )


  file <- toolSubtypeSelect(subtype, files)

  ## if file is .zip uncompress
  extension <- file_ext(basename(file))
  csvName <- paste0(file_path_sans_ext(file), ".csv")
  if (file.exists(csvName)) {
    file <- csvName
  } else if (extension == "zip" && !file.exists(csvName)) {
    tempfolder <- local_tempdir()
    unzip(file, exdir = tempfolder) # use the absolute path to the file in order to unzip when working in the function
    file <- file.path(tempfolder, csvName)
  }

  ## efficient reading of csv file: read only needed columns in the needed type (codes as factor)
  csvcolnames <- colnames(read.table(file, header = TRUE, nrows = 1, sep = ","))

  ## in case data with the years as columns has to be read in start the differentiation here

  if (subtype == "ForestProdTrade") {
    readcolClass <- rep("NULL", length(csvcolnames))
    readcolClass[csvcolnames == "Area.Code" | csvcolnames == "Item.Code" | csvcolnames == "Element.Code"] <- "factor"
    readcolClass[csvcolnames == "Area" | csvcolnames == "Element" |
                   csvcolnames == "Item" | csvcolnames == "Unit"] <- "character"
    readcolClass[csvcolnames == "Value" | csvcolnames == "Year"] <- NA
    fao <- read.table(file,
                      header = FALSE,
                      skip = 1,
                      sep = ",",
                      colClasses = readcolClass,
                      col.names = csvcolnames,
                      quote = "\"",
                      encoding = "latin1")
    names(fao)[names(fao) == "Area.Code"] <- "CountryCode"
    names(fao)[names(fao) == "Area"] <- "Country"
    ## list countries where no respective ISO code is available in a message
    countryandcode <- unique(fao[, c("CountryCode", "Country")])
  } else {
    readcolClass <- rep("NULL", length(csvcolnames))
    readcolClass[csvcolnames == "CountryCode" | csvcolnames == "ItemCode" | csvcolnames == "ElementCode"] <- "factor"
    readcolClass[csvcolnames == "Country" | csvcolnames == "Element" |
                   csvcolnames == "Item" | csvcolnames == "Unit"] <- "character"

    if (subtype == "EmisLuTotal") {
      readcolClass[csvcolnames == "Flag" | csvcolnames == "ElementGroup"] <- NA
      readcolClass[csvcolnames == "Value" | csvcolnames == "Year"] <- NA
    } else if (subtype == "Fodder") {
      readcolClass[csvcolnames == "Value" | csvcolnames == "Year"] <- "character"
      csvcolnames <- csvcolnames[-grep("NULL", readcolClass)]
    } else {
      readcolClass[csvcolnames == "Value" | csvcolnames == "Year"] <- NA
      csvcolnames <- csvcolnames[-grep("NULL", readcolClass)]
    }

    fao <- fread(input = file,
                 header = FALSE,
                 skip = 1,
                 sep = ",",
                 colClasses = readcolClass,
                 col.names = csvcolnames,
                 quote = "\"",
                 encoding = "Latin-1",
                 showProgress = FALSE)
    if (all(!is.factor(fao$CountryCode))) fao$CountryCode <- as.factor(fao$CountryCode)
    fao$Value <- as.numeric(fao$Value)
    # list countries where no respective ISO code is available in a message
    countryandcode <- unique(fao[, c("CountryCode", "Country")])
  }

  # collect the countries that do not exist in the data
  faoIsoFaoCode <- toolGetMapping("FAOiso_faocode.csv", where = "mrcommons")
  notIncl <- countryandcode$Country[!countryandcode$CountryCode %in% faoIsoFaoCode$CountryCode]
  notInclCoun <- notIncl[!grepl("(Total)", notIncl)]
  if (length(notInclCoun) > 0) {
    vcat(1, "The following countries were not included due to missing ISO codes:",
         "\n", paste(notInclCoun, "\n"), "-> Consider an update of FAOiso_faocode.csv", "\n")
 }
  fao <- fao[fao$CountryCode %in% faoIsoFaoCode$CountryCode, ]
  gc()
  fao$ISO <- fao$CountryCode
  # becomes necessary because data is now loaded as .csv
  rownames(faoIsoFaoCode) <- as.character(faoIsoFaoCode$CountryCode)
  levels(fao$ISO) <- as.character(faoIsoFaoCode[levels(fao$CountryCode), "ISO3"])


  ### convert some units
  replace <- fao$Unit == "1000 tonnes"
  if (any(replace)) {
    fao$Value[replace] <- fao$Value[replace] * 1000
    fao$Unit[replace] <- "tonnes"
  }

  replace <- fao$Unit == "1000 Head"
  if (any(replace)) {
    fao$Value[replace] <- fao$Value[replace] * 1000
    fao$Unit[replace] <- "Head"
  }

  replace <- fao$Unit == "1000"
  if (any(replace)) {
    fao$Value[replace] <- fao$Value[replace] * 1000
    fao$Unit[replace] <- "number"
  }

  replace <- fao$Unit == "1000 Ha"
  if (any(replace)) {
    fao$Value[replace] <- fao$Value[replace] * 1000
    fao$Unit[replace] <- "ha"
  }

  ### use ElementShort or a combination of Element and Unit instead of ElementCode
  faoElementShort <- toolGetMapping("FAOelementShort.csv", where = "mrcommons")

  elementShort <- faoElementShort

  ## make ElementShort a combination of Element and Unit, replace special characters, and substitute several _ by one
  fao$ElementShort <- gsub("_{1,}", "_",
                           paste0(gsub("[\\.,;?\\+& \\/\\-]", "_", fao$Element, perl = TRUE),
                                  "_(", gsub("[\\.,;\\+& \\-]", "_", fao$Unit, perl = TRUE), ")"), perl = TRUE)
  ### replace ElementShort with the entries from ElementShort if the Unit is the same
  elementShort <- elementShort[elementShort$ElementCode %in% fao$ElementCode, ]

  if (length(elementShort) > 0) {
    for (i in seq_len(nrow(elementShort))) {
      fao$ElementShort[fao$ElementCode == elementShort[i, "ElementCode"]
                       & fao$Unit == elementShort[i, "Unit"]] <- as.character(elementShort[i, "ElementShort"])
    }
  }

  # remove accent in Mate to avoid problems
  # remove other strange names
  fao$Item <- gsub("\u00E9", "e", fao$Item, perl = TRUE)
  fao$Item <- gsub("\n + (Total)", " + (Total)", fao$Item, fixed = TRUE)
  fao$ItemCodeItem <- paste0(fao$ItemCode, "|", gsub("\\.", "", fao$Item, perl = TRUE))

  gc()

  faoMag <- as.magpie(fao[, c("Year", "ISO", "ItemCodeItem", "ElementShort", "Value")],
                       temporal = 1,
                       spatial = 2,
                       datacol = 5)
  if (subtype == "EmisAgBurnCropResid" || subtype == "EmisAgCropResid" || subtype == "EmisLuForest") {
    getNames(faoMag, dim = 1) <- gsub("\\r", "", getNames(faoMag, dim = 1))
  }

  rm(fao)
  gc()

  faoMag <- magpiesort(faoMag)

  return(faoMag)
}
