#' @importFrom utils download.file tail unzip

downloadFAO_online <- function(subtype) {
  
  links <- c(CBCrop="CommodityBalances_Crops_E_All_Data.zip",
             CBLive="CommodityBalances_LivestockFish_E_All_Data.zip",
             Crop="Production_Crops_E_All_Data.zip",
             CropProc="Production_CropsProcessed_E_All_Data.zip",
             #Fbs="FoodBalanceSheets_E_All_Data.zip", #should not be used, use CB and FS or calcFAOharmonized() instead
             Fbs="FoodBalanceSheets_E_All_Data.zip", 
             Fertilizer="Environment_Fertilizers_E_All_Data.zip",
             #Fodder="Fodder.csv",
             FoodSecurity="Food_Security_Data_E_All_Data.zip",
             FSCrop="FoodSupply_Crops_E_All_Data.zip",
             FSLive="FoodSupply_LivestockFish_E_All_Data.zip",
             #Land="Resources_Land_E_All_Data.zip",
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
  
  settings <- list( CBCrop= list(title = "Food Balance: Commodity Balances - Crops Primary Equivalent.",
                                   url = "CommodityBalances_Crops_E_All_Data.zip"),
                    CBLive= list(title = "Food Balance: Commodity Balances - Livestock and Fish Primary Equivalent",
                                   url = "CommodityBalances_LivestockFish_E_All_Data.zip")
                    )
  
  meta <- toolSubtypeSelect(subtype,settings)
  
  # add base link (preserve 'links' as named vector by using '[] <-')
  links[] <- paste0("http://fenixservices.fao.org/faostat/static/bulkdownloads/",links)
  
  fnames <- sapply(links, function(x){tail(strsplit(x, split = "/")[[1]], 1)})
  
  ### execute downloading
  if (is.null(subtype)) {
    lapply(1:length(links), FUN = function(x){ download.file(links[x], destfile=fnames[x], mode="wb")})
  } else {
    download.file(links[[subtype]], destfile=fnames[[subtype]], mode="wb")
  }

  ###  unzip files
  #zipfiles <- list.files(pattern=".zip$")
  #lapply(zipfiles, unzip)
  #lapply(zipfiles, unlink)

}