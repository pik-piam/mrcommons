#' @importFrom utils download.file tail unzip

downloadFAO_online <- function(subtype) {
  
  settings <- list(CBCrop            = list(title = "Food Balance: Commodity Balances - Crops Primary Equivalent.",
                                              url = "CommodityBalances_Crops_E_All_Data.zip"),
                CBLive               = list(title = "Food Balance: Commodity Balances - Livestock and Fish Primary Equivalent",
                                              url = "CommodityBalances_LivestockFish_E_All_Data.zip"),
                Crop                 = list(title = "",
                                              url = "Production_Crops_E_All_Data.zip"),
                CropProc             = list(title = "",
                                              url = "Production_CropsProcessed_E_All_Data.zip"),
                #Fbs                 = list(title = "", #should not be used, use CB and FS or calcFAOharmonized() instead
                #                             url = "FoodBalanceSheets_E_All_Data.zip"), 
                Fbs                  = list(title = "",
                                              url = "FoodBalanceSheets_E_All_Data.zip"), 
                Fertilizer           = list(title = "",
                                              url = "Environment_Fertilizers_E_All_Data.zip"),
                #Fodder              = list(title = "",
                #                             url = "Fodder.csv",
                FoodSecurity         = list(title = "",
                                              url = "Food_Security_Data_E_All_Data.zip"),
                FSCrop               = list(title = "",
                                              url = "FoodSupply_Crops_E_All_Data.zip"),
                FSLive               = list(title = "",
                                              url = "FoodSupply_LivestockFish_E_All_Data.zip"),
                Land                 = list(title = "",
                                              url = "Inputs_LandUse_E_All_Data_(Normalized).zip"), # old source file: Resources_Land_E_All_Data.zip
                LiveHead             = list(title = "",
                                              url = "Production_Livestock_E_All_Data.zip"),
                LivePrim             = list(title = "",
                                              url = "Production_LivestockPrimary_E_All_Data.zip"),
                LiveProc             = list(title = "",
                                              url = "Production_LivestockProcessed_E_All_Data.zip"),
                Pop                  = list(title = "",
                                              url = "Population_E_All_Data.zip"),
                PricesProducerAnnual = list(title = "",
                                              url = "Prices_E_All_Data.zip"),
                PricesProducerAnnualLCU = list(title = "",
                                              url = "Prices_E_All_Data.zip"),
                
                EmisAgTotal          = list(title = "",
                                              url = "Emissions_Agriculture_Agriculture_total_E_All_Data.zip"),
                EmisAgBurnCropResid  = list(title = "",
                                              url = "Emissions_Agriculture_Burning_crop_residues_E_All_Data.zip"),
                EmisAgBurnSavanna    = list(title = "",
                                              url = "Emissions_Agriculture_Burning_Savanna_E_All_Data.zip"),
                EmisAgCropResid      = list(title = "",
                                              url = "Emissions_Agriculture_Crop_Residues_E_All_Data.zip"),
                EmisAgCultOrgSoil    = list(title = "",
                                              url = "Emissions_Agriculture_Cultivated_Organic_Soils_E_All_Data.zip"),
                EmisAgEnergy         = list(title = "",
                                              url = "Emissions_Agriculture_Energy_E_All_Data.zip"),
                EmisAgEntericFerment = list(title = "",
                                              url = "Emissions_Agriculture_Enteric_Fermentation_E_All_Data.zip"),
                EmisAgManureSoil     = list(title = "",
                                              url = "Emissions_Agriculture_Manure_applied_to_soils_E_All_Data.zip"), 
                EmisAgManurePasture  = list(title = "",
                                              url = "Emissions_Agriculture_Manure_left_on_pasture_E_All_Data.zip"),
                EmisAgManureManag    = list(title = "",
                                              url = "Emissions_Agriculture_Manure_Management_E_All_Data.zip"),
                EmisAgRiceCult       = list(title = "",
                                              url = "Emissions_Agriculture_Rice_Cultivation_E_All_Data.zip"),
                EmisAgSynthFerti     = list(title = "",
                                              url = "Emissions_Agriculture_Synthetic_Fertilizers_E_All_Data.zip"),
                
                EmisLuBurnBiomass    = list(title = "",
                                              url = "Emissions_Land_Use_Burning_Biomass_E_All_Data.zip"),
                EmisLuCrop           = list(title = "",
                                              url = "Emissions_Land_Use_Cropland_E_All_Data.zip"),
                EmisLuForest         = list(title = "",
                                              url = "Emissions_Land_Use_Forest_Land_E_All_Data.zip"),
                EmisLuGrass          = list(title = "",
                                              url = "Emissions_Land_Use_Grassland_E_All_Data.zip"),
                EmisLuTotal          = list(title = "",
                                              url = "Emissions_Land_Use_Land_Use_Total_E_All_Data.zip"),
                
                ValueOfProd     = list(title = "",
                                         url = "Value_of_Production_E_All_Data.zip"),
                ForestProdTrade = list(title = "",
                                        url = "Forestry_E_All_Data_(Normalized).zip")
  )
  
  
  meta <- toolSubtypeSelect(subtype,settings)
  
  # xml file describing the FAO data of the bulk download
  # download.file(url = "http://fenixservices.fao.org/faostat/static/bulkdownloads/datasets_E.xml", destfile = "FAO_datasets_E.xml")
  # download.file(url = "http://fenixservices.fao.org/faostat/static/bulkdownloads/datasets_E.json", destfile = "FAO_datasets_E.json")
  # sort(xml_text(xml_find_all(x, "//FileLocation")))
  # http://www.fao.org/es/faodef/fdef11e.htm
  # http://www.fao.org/3/ca7570en/ca7570en.pdf
  # http://fenixservices.fao.org/faostat/static/releasecalendar/Default.aspx
  
  
  download.file(paste0("http://fenixservices.fao.org/faostat/static/bulkdownloads/",meta$url), destfile=meta$url, mode="wb")

  # Compose meta data by adding elements that are the same for all subtypes.
  return(list(url           = meta$url,
              doi           = "not available",
              title         = meta$title,
              author        = "FAOSTAT",
              version       = "not available",
              release_date  = "2020-12-03",
              license       = "Creative Commons Attribution-NonCommercial-ShareAlike 3.0 IGO (CC BY-NC- SA 3.0 IGO)",
              reference     = "not available")
  )
  

}