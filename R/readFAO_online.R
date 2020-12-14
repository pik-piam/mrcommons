#' Read FAO_online
#' 
#' Read in FAO data that has been downloaded from the FAOSTAT website.
#' Files with exception of fodder.csv are aquired from:
#' http://fenixservices.fao.org/faostat/static/bulkdownloads/
#' 
#' Update 23-Jan-2017 - Added FAO Forestry production and trade data (Abhi)
#' 
#' 
#' @param subtype Type of FAO data that should be read. Available types are:
#' \itemize{ 
#' \item \code{CBCrop}: Commodity Balance Crop (CommodityBalances_Crops_E_All_Data.zip)
#' \item \code{CBLive}: Commoditiy Balance Livestock (CommodityBalances_LivestockFish_E_All_Data.zip)
#' \item \code{Crop}: Production Crops ("Production_Crops_E_All_Data.zip")
#' \item \code{CropProc}: Production Crops Processed ("Production_CropsProcessed_E_All_Data.zip")
#' \item \code{Fbs}: Food Balance Sheet ("FoodBalanceSheets_E_All_Data.zip")
#' \item \code{Fertilizer}: Fertilizer ("Resources_Fertilizers_E_All_Data.zip")
#' \item \code{Fodder}: Fodder (data that has been manually downloaded from the FAOSTAT website as
#' seperate .xls files via a search for "forage" and "fodder" withing
#' Production-Crops. These datasets have been added together to a "Fodder.csv" file)
#' \item \code{FoodSecurity}: Food Security Data ("Food_Security_Data_E_All_Data.zip")
#' \item \code{FSCrop}: Food Supply Crops ("FoodSupply_Crops_E_All_Data.zip")
#' \item \code{FSLive}: Food Supply Livestock ("FoodSupply_LivestockFish_E_All_Data.zip")
#' \item \code{Land}: Land ("Resources_Land_E_All_Data.zip")
#' \item \code{LiveHead}: Production Live Animals ("Production_Livestock_E_All_Data.zip")
#' \item \code{LivePrim}: Production Livestock Primary ("Production_LivestockPrimary_E_All_Data.zip")
#' \item \code{LiveProc}: Production Livestock Processed ("Production_LivestockProcessed_E_All_Data.zip")
#' \item \code{Pop}: Population ("Population_E_All_Data.zip") 
#' \item \code{ForestProdTrade}: Forestry Production and Trade ("Forestry_E_All_Data_(Normalized).zip")
#' \item \code{PricesProducerAnnual}: Producer Prices - Annual ("Prices_E_All_Data.zip")
#' \item \code{PricesProducerAnnualLCU}: Producer Prices - Annual in LCU ("Prices_E_All_Data.zip")
#' \item \code{ValueOfProd}: Value of Agricultural Production ("Value_of_Production_E_All_Data.zip") 
#' }
#' @return FAO data as MAgPIE object
#' @author Ulrich Kreidenweis, Abhijeet Mishra, Mishko Stevanovic
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#'   \dontrun{ a <- readSource("FAO","Crop")
#'   }
#' @importFrom tools file_ext
#' @importFrom data.table fread
#' @importFrom utils unzip
#' @importFrom tools file_path_sans_ext
#' @importFrom tidyr pivot_longer

readFAO_online <- function(subtype) {
  
  # ---- Define subtypes and corresponding files ---- 
  
  files <- c(CBCrop="CommodityBalances_Crops_E_All_Data.zip",
             CBLive="CommodityBalances_LivestockFish_E_All_Data.zip",
             Crop="Production_Crops_E_All_Data.zip",
             CropProc="Production_CropsProcessed_E_All_Data.zip",
             #Fbs="FoodBalanceSheets_E_All_Data.zip", #should not be used, use CB and FS or calcFAOharmonized() instead
             Fbs="FoodBalanceSheets_E_All_Data.zip", 
             Fertilizer="Environment_Fertilizers_E_All_Data.zip",
             Fodder="Fodder.csv",
             FoodSecurity="Food_Security_Data_E_All_Data.zip",
             FSCrop="FoodSupply_Crops_E_All_Data.zip",
             FSLive="FoodSupply_LivestockFish_E_All_Data.zip",
             Land="Resources_Land_E_All_Data.zip",
             LiveHead="Production_Livestock_E_All_Data.zip",
             LivePrim="Production_LivestockPrimary_E_All_Data.zip",
             LiveProc="Production_LivestockProcessed_E_All_Data.zip",
             Pop="Population_E_All_Data.zip",
             PricesProducerAnnual="Prices_E_All_Data.zip",
             PricesProducerAnnualLCU="Prices_E_All_Data.zip",
             
             
             EmisAgTotal="Emissions_Agriculture_Agriculture_total_E_All_Data.zip",
             EmisAgBurnCropResid="Emissions_Agriculture_Burning_crop_residues_E_All_Data.zip",
             EmisAgBurnSavanna="Emissions_Agriculture_Burning_Savanna_E_All_Data.zip",
             EmisAgCropResid="Emissions_Agriculture_Crop_Residues_E_All_Data.zip",
             EmisAgCultOrgSoil="Emissions_Agriculture_Cultivated_Organic_Soils_E_All_Data.zip",
             EmisAgEnergy="Emissions_Agriculture_Energy_E_All_Data.zip",
             EmisAgEntericFerment="Emissions_Agriculture_Enteric_Fermentation_E_All_Data.zip",
             EmisAgManureSoil="Emissions_Agriculture_Manure_applied_to_soils_E_All_Data.zip", 
             EmisAgManurePasture="Emissions_Agriculture_Manure_left_on_pasture_E_All_Data.zip",
             EmisAgManureManag="Emissions_Agriculture_Manure_Management_E_All_Data.zip",
             EmisAgRiceCult="Emissions_Agriculture_Rice_Cultivation_E_All_Data.zip",
             EmisAgSynthFerti="Emissions_Agriculture_Synthetic_Fertilizers_E_All_Data.zip",
             
             EmisLuBurnBiomass="Emissions_Land_Use_Burning_Biomass_E_All_Data.zip",
             EmisLuCrop="Emissions_Land_Use_Cropland_E_All_Data.zip",
             EmisLuForest="Emissions_Land_Use_Forest_Land_E_All_Data.zip",
             EmisLuGrass="Emissions_Land_Use_Grassland_E_All_Data.zip",
             EmisLuTotal="Emissions_Land_Use_Land_Use_Total_E_All_Data.zip",
             
             ValueOfProd="Value_of_Production_E_All_Data.zip",
             ForestProdTrade="Forestry_E_All_Data_(Normalized).zip")
  
  
  file <- toolSubtypeSelect(subtype,files)
  
  # If the name of the csv file is longer than the zip file name add the missing part here.
  # This is the case for "Pop": the csv file with the same name as the zip file ("Population_E_All_Data") has no header. 
  # There is a second file ("Population_E_All_Data_NOFLAG") with header (and without the superfluous flags).
  affix <- ifelse(subtype=="Pop","_NOFLAG","")
  
  ## if file is .zip uncompress
  extension <- file_ext(basename(file))
  csv_name <- paste0(file_path_sans_ext(file),affix, ".csv")
  if (file.exists(csv_name)) {
    file <- csv_name
  } else if(extension=="zip" & !file.exists(csv_name)){
    unzip(file, exdir = tempdir())                       # use the absolute path to the file in order to unzip when working in the function
    file <- paste0(tempdir(), "/",csv_name)
    on.exit(file.remove(file))
  }
  
  # ---- Select columns to be read from file and read file ---- 
  
  ## efficient reading of csv file: read only needed columns in the needed type (codes as factor)
  csvcolnames <- colnames(read.table(file, header=T, nrows=1, sep=","))
  
  # check if data is in long or wide format
  long <- ifelse("Year" %in% csvcolnames,TRUE,FALSE)
  
  
  # define vector with types corresponding to the columns in the file to be read
  readcolClass <- rep("NULL",length(csvcolnames))
  readcolClass[csvcolnames %in% c("Area.Code","CountryCode","Item.Code","ItemCode","Element.Code","ElementCode")] <- "factor" 
  readcolClass[csvcolnames %in% c("Area","Country","Element","Item","Unit")] <- "character"
  readcolClass[csvcolnames %in% c("Value","Year")] <- NA
  
  subtypes_tested <- c("CBCrop","CBLive","Crop","CropProc","ForestProdTrade",
                       "EmisAgBurnCropResid","Fertilizer","FoodSecurity",
                       "FSCrop","FSLive","LiveHead","LivePrim","LiveProc","Pop","PricesProducerAnnual")
    
  if (subtype %in% subtypes_tested) {
    if (!long) readcolClass[grepl("Y[0-9]{4}$",csvcolnames)] <- NA
    FAO <- fread(input=file, header=F, skip=1, sep=",", colClasses=readcolClass, col.names= csvcolnames[is.na(readcolClass) | readcolClass != "NULL"], quote = "\"", encoding = "Latin-1", showProgress = FALSE)
    # from wide to long (move years from individual columns into one column)
    if (!long) FAO <- pivot_longer(FAO,cols = starts_with("Y"),names_to = "Year", names_pattern = "Y(.*)", names_transform = list("Year" = as.integer), values_to = "Value")
    names(FAO)[names(FAO) == "Area.Code"] <- "CountryCode"
    names(FAO)[names(FAO) == "Area"] <- "Country"
    names(FAO) <- gsub("\\.","",names(FAO))
  } else {
    
    if(subtype=="EmisLuTotal"){
      readcolClass[csvcolnames=="Flag" | csvcolnames=="ElementGroup"] <- NA
    } else if(subtype=="Fodder"){
      readcolClass[csvcolnames=="Value" | csvcolnames=="Year"] <- "character"
      csvcolnames <- csvcolnames[-grep("NULL", readcolClass)]
    } else {
      csvcolnames <- csvcolnames[-grep("NULL", readcolClass)]
    }
  
    FAO <- fread(input=file, header=F, skip=1, sep=",", colClasses=readcolClass, col.names= csvcolnames, quote = "\"", encoding = "Latin-1", showProgress = FALSE)
    if(all(!is.factor(FAO$CountryCode))) FAO$CountryCode <- as.factor(FAO$CountryCode)
    FAO$Value <- as.numeric(FAO$Value)
  }
  
  # ---- Identify unknown countries ----
  
  # Load FAO-ISO-mapping
  FAOiso_faocode <- toolGetMapping("FAOiso_faocode.csv", where="mrcommons")
  # Find countries that are not included in the FAO-ISO-mapping
  not_incl <- FAO[!FAO$CountryCode %in% FAOiso_faocode$CountryCode,c("CountryCode","Country")]
  # find unique list of countries using match avoiding time consuming 'unique()' command. Old: countryandcode <- unique(FAO[,c("CountryCode","Country")])
  not_incl <- not_incl[match(levels(not_incl$CountryCode),not_incl$CountryCode),]
  # ignore countries that are aggregates (CountryCode >= 5000, formerly had '(Total)' in their name)
  not_incl <- not_incl[as.numeric(levels(not_incl$CountryCode))[not_incl$CountryCode]<5000,]
  # remove rows containing NA
  not_incl <- na.omit(not_incl)
  if (!0 %in% dim(not_incl)) {
    vcat(1,"The following countries were not included due to missing ISO codes:",
         "\n", paste0(not_incl$Country,"\n"),"-> Consider an update of FAOiso_faocode.csv", "\n") }
  # Remove countries from data that are not included in the FAO-ISO-mapping
  FAO <- FAO[FAO$CountryCode %in% FAOiso_faocode$CountryCode,]
  gc()
  FAO$ISO <- FAO$CountryCode
  rownames(FAOiso_faocode) <- as.character(FAOiso_faocode$CountryCode) # becomes necessary because data is now loaded as .csv
  levels(FAO$ISO) <- as.character(FAOiso_faocode[levels(FAO$CountryCode),"ISO3"])
  
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
  gc()
  
  FAO <- magpiesort(FAO)

  return(FAO)
}
