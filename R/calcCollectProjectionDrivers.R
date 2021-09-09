#' Collect Projection Drivers
#' @param driver Driver to collect. Defaults to "all".
#' @importFrom magclass getNames<- 
calcCollectProjectionDrivers <- function(driver = "all"){
  
  if(driver != "all"){
    all <- calcOutput("CollectProjectionDrivers", aggregate = FALSE)
    combined <- collapseNames(all[,,driver])
  } else {

    ## add education indicators
    pop_ssp_sres <- calcOutput("Population", aggregate = FALSE)
    gdp_ssp_sres <- calcOutput("GDPppp", aggregate = FALSE)
    
    # Convert from 2005 Int$PPP to 2005 US$MER, and use regional averages when conversion factors are missing
    regmap <- toolGetMapping("regionmappingH12.csv") %>% 
      tibble::as_tibble() %>% 
      dplyr::select("iso3c" = .data$CountryCode, "region" = .data$RegionCode)

    gdp_ssp_sres_mer <- GDPuc::convertGDP(gdp_ssp_sres,
                                          unit_in = "constant 2005 Int$PPP",
                                          unit_out = "constant 2005 US$MER",
                                          with_regions = regmap,
                                          replace_NAs = "regional_average")
    
    getNames(gdp_ssp_sres_mer, dim = 1) <-  gsub("gdp", "gdpmer", getNames(gdp_ssp_sres_mer, dim=1))
    
    urban_shr_ssp <- calcOutput("Urban", 
                                UrbanCalib = "past", 
                                UrbanPast = "WDI", 
                                UrbanFuture = "SSP",
                                aggregate = FALSE) 
    urban_ssp <- urban_shr_ssp*pop_ssp_sres[,,getNames(urban_shr_ssp)]
    getNames(urban_ssp, dim = 1) <- gsub("pop", "urban", getNames(urban_ssp, dim=1))
    
    # Demographics
    #Lutz<-calcOutput("Demography",education=FALSE,aggregate=FALSE)  #No division into education groups (due to eductaion=FALSE)
    #population <- dimSums(Lutz, dim=c("age","sex"))
    #population<-add_dimension(x = population,dim = 3.1,add="indicator",nm = "population")
    
    combined<-mbind(
      pop_ssp_sres,
      gdp_ssp_sres,
      gdp_ssp_sres_mer,
      urban_ssp)
      #population)

  getNames(combined) <-  sub("_", ".", getNames(combined))  
      }
  
  if (driver == "gdp"){
    unit <- "Mio USD 05"  
  } else if(driver == "pop") {
    unit <- "Mio people"
  } else if(driver == "urban") {
    unit <- "Mio people"
  } else {
    unit <- "population: Mio people, gdp: Million USD, urban population: Mio people"
  }
  
  return(list(x = combined,
              weight = NULL,
              unit = unit,
              description = "collects all data necessary to create demand projections and brings them into a joint format"))
}
