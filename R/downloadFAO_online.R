#' Download FAO data 
#' 
#' Downloads the latest data and meta data form the FAOStat website.
#' 
#' @param subtype Type of FAO data that should be read.
#' 
#' @importFrom utils download.file unzip person
#' @importFrom XML xmlToDataFrame

downloadFAO_online <- function(subtype) {
  
  # DEFINITION AND CLASSIFICATION OF COMMODITIES: 
  #    http://www.fao.org/es/faodef/fdef11e.htm
  # LICENSING INFORMATION 
  #    http://www.fao.org/3/ca7570en/ca7570en.pdf
  # Meta data printed as table
  #    http://fenixservices.fao.org/faostat/static/releasecalendar/Default.aspx

  files <- c(
    CBCrop                  = "CommodityBalances_Crops_E_All_Data_(Normalized).zip",
    CBLive                  = "CommodityBalances_LivestockFish_E_All_Data_(Normalized).zip",
    EmisAgTotal             = "Emissions_Agriculture_Agriculture_total_E_All_Data_(Normalized).zip",
    EmisAgBurnCropResid     = "Emissions_Agriculture_Burning_crop_residues_E_All_Data_(Normalized).zip",
    EmisAgBurnSavanna       = "Emissions_Agriculture_Burning_Savanna_E_All_Data_(Normalized).zip",
    EmisAgCropResid         = "Emissions_Agriculture_Crop_Residues_E_All_Data_(Normalized).zip",
    EmisAgCultOrgSoil       = "Emissions_Agriculture_Cultivated_Organic_Soils_E_All_Data_(Normalized).zip",
    EmisAgEnergy            = "Emissions_Agriculture_Energy_E_All_Data_(Normalized).zip",
    EmisAgEntericFerment    = "Emissions_Agriculture_Enteric_Fermentation_E_All_Data_(Normalized).zip",
    EmisAgManureSoil        = "Emissions_Agriculture_Manure_applied_to_soils_E_All_Data_(Normalized).zip",
    EmisAgManurePasture     = "Emissions_Agriculture_Manure_left_on_pasture_E_All_Data_(Normalized).zip",
    EmisAgManureManag       = "Emissions_Agriculture_Manure_Management_E_All_Data_(Normalized).zip",
    EmisAgRiceCult          = "Emissions_Agriculture_Rice_Cultivation_E_All_Data_(Normalized).zip",
    EmisAgSynthFerti        = "Emissions_Agriculture_Synthetic_Fertilizers_E_All_Data_(Normalized).zip",
    EmisLuBurnBiomass       = "Emissions_Land_Use_Burning_Biomass_E_All_Data_(Normalized).zip",
    EmisLuCrop              = "Emissions_Land_Use_Cropland_E_All_Data_(Normalized).zip",
    EmisLuForest            = "Emissions_Land_Use_Forest_Land_E_All_Data_(Normalized).zip",
    EmisLuGrass             = "Emissions_Land_Use_Grassland_E_All_Data_(Normalized).zip",
    EmisLuTotal             = "Emissions_Land_Use_Land_Use_Total_E_All_Data_(Normalized).zip",
    Fertilizer              = "Environment_Fertilizers_E_All_Data_(Normalized).zip",
    #Fodder                 = "",
    FoodSecurity            = "Food_Security_Data_E_All_Data_(Normalized).zip",
    Fbs                     = "FoodBalanceSheets_E_All_Data_(Normalized).zip",
    FSCrop                  = "FoodSupply_Crops_E_All_Data_(Normalized).zip",
    FSLive                  = "FoodSupply_LivestockFish_E_All_Data_(Normalized).zip",
    ForestProdTrade         = "Forestry_E_All_Data_(Normalized).zip",
    Land                    = "Inputs_LandUse_E_All_Data_(Normalized).zip",
    Pop                     = "Population_E_All_Data_(Normalized).zip",
    PricesProducerAnnual    = "Prices_E_All_Data_(Normalized).zip",
    PricesProducerAnnualLCU = "Prices_E_All_Data_(Normalized).zip",
    Crop                    = "Production_Crops_E_All_Data_(Normalized).zip",
    CropProc                = "Production_CropsProcessed_E_All_Data_(Normalized).zip",
    LiveHead                = "Production_Livestock_E_All_Data_(Normalized).zip",
    LivePrim                = "Production_LivestockPrimary_E_All_Data_(Normalized).zip",
    LiveProc                = "Production_LivestockProcessed_E_All_Data_(Normalized).zip",
    ValueOfProd             = "Value_of_Production_E_All_Data_(Normalized).zip"
  ) 

  file <- toolSubtypeSelect(subtype,files)
  
  # Download meta data (e.g. name, description, release date, file path) for all FAO data sets currently available
  fao_meta_xmlfile <- "FAO_datasets_E.xml"
  download.file(url = "http://fenixservices.fao.org/faostat/static/bulkdownloads/datasets_E.xml", destfile = fao_meta_xmlfile)
  fao_meta <- xmlToDataFrame(fao_meta_xmlfile,stringsAsFactors = F)
  unlink(fao_meta_xmlfile)
  
  # extract the data set for the selected subtype by searching for the file name
  fao_meta <- fao_meta[grepl(pattern = file,fao_meta$FileLocation,fixed = T),]

  # download the data
  download.file(fao_meta$FileLocation, destfile=file, mode="wb")

  # Compose meta data by adding elements that are the same for all subtypes.
  return(list(url           = fao_meta$FileLocation,
              doi           = "not available",
              title         = fao_meta$DatasetName,
              author        = person(fao_meta$Contact, email=fao_meta$Email),
              version       = "not available",
              release_date  = fao_meta$DateUpdate,
              description   = fao_meta$DatasetDescription,
              license       = "Creative Commons Attribution-NonCommercial-ShareAlike 3.0 IGO (CC BY-NC- SA 3.0 IGO)",
              reference     = "not available")
  )

}