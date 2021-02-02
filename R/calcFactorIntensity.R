#' @title calcFactorIntensity
#' @description Calculates factor intensity for labour and/or capital from USDA (Inputs share) and FAO (Value of Production)in USD05 per ton.
#' Capital intensity and requirements can also be calculated from FAO's CapitalStock database.
#'
#' 
#'
#' @param output needed outputs. It can be either "intensities" (Capital/Labour factor intensities), "requirements" (Capital Stock requirements per ton), and "CapitalShare" for "USDA" method. For the "CapitalStock" method only "intensities" and "requirements" outputs supported.  
#' @param method "USDA" or "CapitalStock"
#' @return magpie object of the factor requirements intensity or factor intensity in USD05/ton per crop, or capital share fraction.
#' @author Edna J. Molina Bacca
#' @importFrom luscale speed_aggregate
#' @importFrom dplyr intersect
#' @seealso \code{\link{calcOutput}}
#' @examples
#'
#' \dontrun{
#' a <- calcOutput("FactorIntensity")

#' }
#'
calcFactorIntensity <- function(output="intensities", method = "USDA") {

 
  if (method == "USDA") { #using USDA mehod
  inputs<-c("revenue" ,"Machinery", "AG_Land", "AG_Labour", "Materials_Crops")
  TFP_shares_raw<-readSource("TFP_USDA")[,,inputs] # shares of inputs from USDA TFP data set writen as average decades
  TFP_shares <- magpiesort(new.magpie(cells_and_regions = getCells(TFP_shares_raw),names = getNames(TFP_shares_raw),years = c(getYears(TFP_shares_raw,as.integer=TRUE),(getYears(TFP_shares_raw,as.integer = TRUE)+5))))
  
  for (y in getYears(TFP_shares_raw,as.integer = TRUE)){
    TFP_shares[,y,] <- TFP_shares_raw[,y,]
    TFP_shares[,y+5,] <- TFP_shares_raw[,y,]
  }
  
  crop_prod_dm_All  <- calcOutput("Production",products="kcr", aggregate = FALSE, attributes="dm") #mio. ton
  getNames(crop_prod_dm_All)<-gsub("\\..*","",getNames(crop_prod_dm_All))
  crop_prod_dm  <-dimSums(crop_prod_dm_All,dim=3)
  livestock_prod_dm <- dimSums(calcOutput("Production",products="kli", aggregate = FALSE, attributes="dm"),dim=3)

  #Share between livestock and crop production
  crop_share_production<-crop_prod_dm/(crop_prod_dm+livestock_prod_dm)

  livestock_input<-c("revenue" ,"Machinery", "AG_Labour")
  crop_only<-c("AG_Land","Materials_Crops")

  years<-intersect(getYears(TFP_shares),getYears(crop_share_production))
   
  # calculated fraction of production coming from either machinery (capital) or Labor
  fraction_capital <- crop_share_production[,years,]*TFP_shares[,years,"Machinery"]/(crop_share_production[,years,]*(dimSums(TFP_shares[,years,livestock_input],dim=3))+dimSums(TFP_shares[,years,crop_only],dim=3))
  fraction_labor <- crop_share_production[,years,]*TFP_shares[,years,"AG_Labour"]/(crop_share_production[,years,]*(dimSums(TFP_shares[,years,livestock_input],dim=3))+dimSums(TFP_shares[,years,crop_only],dim=3))

  VoP_crops<-calcOutput("VoP_crops",output="absolute",units="USD05",aggregate=FALSE) #mio. USD05

  gnames<-intersect(getNames(VoP_crops),getNames(crop_prod_dm_All))
  gyears<-intersect(getYears(VoP_crops),getYears(crop_prod_dm_All))
  gcells<-intersect(getCells(VoP_crops),getCells(crop_prod_dm_All))
  
  # Value of production per ton produced
  VoP_perTon<- VoP_crops[gcells,gyears,gnames]/crop_prod_dm_All[gcells,gyears,gnames] #USD05/ton
  fyears<-intersect(getYears(fraction_capital),getYears(VoP_perTon))

  
  #Calculation of capital and labor intensities, and Capital share between the two
  Capital_Intensity <- fraction_capital[,fyears,]*VoP_perTon[,fyears,]
  Labour_Intensity <- fraction_labor[,fyears,]*VoP_perTon[,fyears,]
  Share_Capital <- Capital_Intensity / (Capital_Intensity + Labour_Intensity )

  Capital_Intensity[!is.finite(Capital_Intensity)]<-0
  Labour_Intensity[!is.finite(Labour_Intensity)]<-0
  Share_Capital[!is.finite(Share_Capital)]<-0

  
   #assuming a 4% interest rate and 5% depreciation
   x <- if (output == "intensities") magpiesort(mbind(Capital_Intensity,Labour_Intensity)) else if (output == "requirements") Capital_Intensity/(0.04+0.05) else if (output == "CapitalShare") Share_Capital else stop("Output not supported")
   weight <- x
   weight[,,] <- 1
   weight[x == 0]<- 0
   
   }else if (method == "CapitalStock" & output %in% c("intensities","requirements")){
    
    # Fraction of each crop on overall Value of Production (Agriculture, Forestry and Fisheries)
    fraction_VoP_crop<-calcOutput("VoP_crops",output="fraction",aggregate=FALSE)
    
    #Existing capital stocks
    CapitalStocks<-readSource("FAO_online","CapitalStock",convert=TRUE)[,,"22034|Net Capital Stocks (Agriculture, Forestry and Fishing).Value_USD_2015_prices_(millions)"]
    
    years<-intersect(getYears(fraction_VoP_crop),getYears(CapitalStocks))
    region<-intersect(getCells(fraction_VoP_crop),getCells(CapitalStocks))
    
    #Capital stocks per crop
    CapitalStocks_crop<-CapitalStocks[region,years,]*fraction_VoP_crop[region,years,]/(1+0.04)^10 #to bring to USD05
    getNames(CapitalStocks_crop)<-gsub(".*\\.","",getNames(CapitalStocks_crop))
    
    if (output %in% c("requirements","intensities")){
      Production<-calcOutput("Production",products="kcr",aggregate=FALSE)[,,"dm"]
      getNames(Production)<-gsub("\\..*","",getNames(Production))
      
      names<-intersect(getNames(Production),getNames(CapitalStocks_crop))
      years<-intersect(getYears(Production),getYears(CapitalStocks_crop))
      cells<-intersect(getCells(Production),getCells(CapitalStocks_crop))
      
      #Capital stock requirements
      x<-CapitalStocks_crop[cells,years,names]/Production[cells,years,names]
      x[!is.finite(x)]<-0
     
    }else{
      
      stop("Output not supported")
      
    }
    
    #assuming a 4% interest rate and a 5% depreciation rate
    
    x<-if (output== "intensities") x*(0.04+0.05) else if (output== "requirements") x 
    
    weight <- x
    weight[,,] <- 1
    weight [x == 0] <- 0

  }else{
    stop ("Method or output not supported")
  }
  
   units<- if (output %in% c("intensities","requirements")) "USD05/ton" else if(output == "CapitalShare") "fraction"
  
  
  
   return(list(x=x,
               weight=weight,
               mixed_aggregation=NULL,
               unit=units,
               description="Factor Intensities or capital requirements for different crops in USD05 per ton"))
}
