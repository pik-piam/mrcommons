#' @title downloadEDGAR
#' @description download EDGAR emission data
#'
#' @param subtype type in gas for receiving the newest data. type in specific filename for old version 4.31 data
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Benjamin Leon Bodirsky, 
#' @importFrom utils download.file tail unzip bibentry

downloadEDGAR6 <- function(subtype="n2o") {
  
  # Define subtype-specific elements of the meta data. Elements that are common to all subtypes are added further down.
  settings <- list(     n2o = list(title = "EDGAR 6 N2O data inventories by country and emission source",
                                   url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/N2O/v60_GHG_N2O_1970_2018.zip",
                                   doi = "10.1038/s43016-021-00225-9",
                                   version= "6.0"),
                        ch4 = list(title = "EDGAR 6 CH4 data inventories by country and emission source",
                                   url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/CH4/v60_GHG_CH4_1970_2018.zip",
                                   doi = "10.1038/s43016-021-00225-9",
                                   version= "6.0"),
                        # short cycle CO2
                        co2_incl_short = list(title = "EDGAR 6 CO2 (including short cycle emissions) data inventories by country and emission source",
                                              url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/CO2_org_short-cycle_C/v60_GHG_CO2_org_short-cycle_C_1970_2018.zip",
                                              doi = "10.1038/s43016-021-00225-9",
                                              version= "6.0"),
                        # long cycle CO2
                        co2_excl_short = list(title = "EDGAR 6 CO2 (excluding short cycle emisssions) data inventories by country and emission source",
                                              url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/CO2_excl_short-cycle_org_C/v60_GHG_CO2_excl_short-cycle_org_C_1970_2018.zip",
                                              doi = "10.1038/s43016-021-00225-9",
                                              version= "6.0"),
                        n2o = list(title = "EDGAR 6 N2O data inventories by country and emission source",
                                   url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/N2O/v60_GHG_N2O_1970_2018.zip",
                                   doi = "10.1038/s43016-021-00225-9",
                                   version= "6.0"),
                        bc = list(title = "EDGAR 5 BC data inventories by country and emission source",
                                  url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v50_AP/BC/v50_BC_1970_2015.zip",
                                  doi = "10.2760/687800",
                                  version= "5.0"),
                        co = list(title = "EDGAR 5 CO data inventories by country and emission source",
                                  url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v50_AP/CO/v50_CO_1970_2015.zip",
                                  doi = "10.2760/687800",
                                  version= "5.0"),
                        nh3 = list(title = "EDGAR 5 NH3 data inventories by country and emission source",
                                   url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v50_AP/NH3/v50_NH3_1970_2015.zip",
                                   doi = "10.2760/687800",
                                   version= "5.0"),
                        nmvoc = list(title = "EDGAR 5 NMVOC data inventories by country and emission source",
                                     url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v50_AP/NMVOC/v50_NMVOC_1970_2015.zip",
                                     doi = "10.2760/687800",
                                     version= "5.0"),
                        nox = list(title = "EDGAR 5 NOx data inventories by country and emission source",
                                   url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v50_AP/NOx/v50_NOx_1970_2015.zip",
                                   doi = "10.2760/687800",
                                   version= "5.0"),
                        oc = list(title = "EDGAR 5 OC data inventories by country and emission source",
                                  url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v50_AP/OC/v50_OC_1970_2015.zip",
                                  doi = "10.2760/687800",
                                  version= "5.0"),
                        pm10 = list(title = "EDGAR 5 PM10 data inventories by country and emission source",
                                    url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v50_AP/PM10/v50_PM10_1970_2015.zip",
                                    doi = "10.2760/687800",
                                    version= "5.0"),
                        pm25 = list(title = "EDGAR 5 PM25 data inventories by country and emission source",
                                    url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v50_AP/PM2.5/v50_PM2.5_1970_2015.zip",
                                    doi = "10.2760/687800",
                                    version= "5.0"),
                        so2 = list(title = "EDGAR 5 SO2 data inventories by country and emission source",
                                   url = "https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v50_AP/SO2/v50_SO2_1970_2015.zip",
                                   doi = "10.2760/687800",
                                   version= "5.0")
  )
  meta <- toolSubtypeSelect(subtype,settings)
  
  download.file(meta$url, destfile = paste0(subtype,".zip"))
  unzip(paste0(subtype,".zip"))
  unlink(paste0(subtype,".zip"))
  
  if (meta$version=="6.0"){
    meta$reference=bibentry("TechReport",
                            title="EDGAR v6.0 Greenhouse Gas Emissions",
                            author=c(person("Crippa, M."),
                                     person("Guizzardi, D."),
                                     person("Muntean, M"),
                                     person("Schaaf, E"),
                                     person("Lo Vullo, E"),
                                     person("Solazzo, E"),
                                     person("Monforti-Ferrario, F."),
                                     person("Olivier, J.G.J"),
                                     person("Vignati, E ")),
                            year="2021",
                            institution="JRC",
                            url="https://edgar.jrc.ec.europa.eu/index.php/dataset_ghg60",
                            doi="http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b")
    
  } else if (meta$version=="5.0") {
    meta$reference=bibentry("TechReport",
                            title="Fossil CO2 and GHG emissions of all world countries: 2019 report.",
                            author=c(person("Crippa, M."),
                                     person("Guizzardi, D."),
                                     person("Muntean, M"),
                                     person("Schaaf, E"),
                                     person("Lo Vullo, E"),
                                     person("Solazzo, E"),
                                     person("Monforti-Ferrario, F."),
                                     person("Olivier, J.G.J"),
                                     person("Vignati, E ")),
                            year="2019",
                            institution="JRC",
                            url="http://publications.europa.eu/publication/manifestation_identifier/PUB_KJNA29849ENN",
                            doi="10.2760/687800")
  }
  
  # Compose meta data by adding elements that are the same for all subtypes.
  return(list(url           = meta$url,
              doi           = meta$doi,
              title         = meta$title,
              author        = person("Crippa","Monica"),
              version       = meta$version,
              release_date  = "2021-10-11",
              license       = "Creative Commons Attribution 4.0 International (CC BY 4.0) licence", # http://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v60_GHG/copyright.txt
              reference     = meta$reference)
  )
}
