#' @title calcPricesProducer
#' @description producer prices for agricultural products. 05USD ppp 
#' 
#'
#' 
#'
#' @param products either "kcr" or "kcl"
#' @param type type of calculation "FAO" (directly reads the data), VoP
#' calculates as VoP/Production, NULL for "kli" products
#' @return magpie object. prices in year specific annual
#' @author Edna J. Molina Bacca
#' @importFrom magpiesets findset
#' 
#'
#' @seealso \code{\link{calcOutput}}
#' @examples
#'
#' \dontrun{
#' a <- calcOutput("PricesProducer")

#' }
#'
calcPricesProducer <- function(products="kcr",type="VoP") {
  
  #Conversion of current USD MER to 05USDppp
  GDP_ppp <- calcOutput("GDPppp", aggregate = FALSE, FiveYearSteps = FALSE)[, , "gdp_SSP2"]
  GDP_mer <- readSource("WDI", "NY.GDP.MKTP.CD")
  years<-intersect(getYears(GDP_ppp),getYears(GDP_mer))
  GDP_con_mer <- setYears(GDP_mer[, 2005, ] / GDP_mer[, years, ], years)
  GDP_con <- setNames(GDP_con_mer * setYears(GDP_ppp[, 2005, ] / GDP_mer[, 2005, ], NULL),NULL)
  # Read Prices producer with FAO format
  
  prices_prod_FAO <- readSource("FAO_online","PricesProducerAnnual") #USD per ton 
  years <- intersect(getYears(prices_prod_FAO),getYears(GDP_con))
  prices_prod_FAO <- prices_prod_FAO[,years,] * GDP_con[,years,]#USD per ton
  prices_prod_FAO [!is.finite(prices_prod_FAO)] <- 0
  
  getNames(prices_prod_FAO)[getNames(prices_prod_FAO) == "254|Oil palm fruit"] <- "254|Oil, palm fruit"
  
  if (products == "kcr"){
  if (type== "FAO"){#items for aggregation
  mappingFAO<-toolGetMapping("FAO2LUH2MAG_croptypes.csv", type = "sectoral", where = "mrcommons")
  items_intersect<-intersect(getNames(prices_prod_FAO),unique(mappingFAO$ProductionItem))
  
  
  #weight: Production
  weight_p<-collapseNames(readSource("FAO_online","Crop")[,,"production"])
  getNames(weight_p)[getNames(weight_p) == "254|Oil palm fruit"] <- "254|Oil, palm fruit"
  names<-intersect(getNames(weight_p),getNames(prices_prod_FAO[,,items_intersect]))
  years<-intersect(getYears(weight_p),getYears(prices_prod_FAO[,,items_intersect]))
  mappingFAO<-mappingFAO[mappingFAO$ProductionItem %in% names,]

  #Aggregation to magpie objects
  prices_prod_FAO_kcr<-toolAggregate(prices_prod_FAO[,years,names],rel=mappingFAO,from="ProductionItem",to="kcr",weight=weight_p[,years,names],dim=3,wdim=3)[,,"remaining",invert=TRUE]
  missing<-setdiff(findset("kcr"),getNames(prices_prod_FAO_kcr))
  
  #Fill with maiz' value the missing crops (betr,begr,foddr)
  prices_prod_FAO_kcr<-add_columns(prices_prod_FAO_kcr,addnm = missing ,dim = 3.1)
  prices_prod_FAO_kcr[,,missing]<- prices_prod_FAO_kcr[,,"maiz"]
  
  #output
  x<-prices_prod_FAO_kcr
  weight<-collapseNames(calcOutput("Production",products="kcr",aggregate=FALSE,attributes="dm"))
  
  #years and names subseting
  years<-intersect(getYears(weight),getYears(x))
  names<-intersect(getNames(weight),getNames(x))
  
  weight <- weight[,years,names]
  x <- x[,years,names]
  }else if (type== "VoP"){
   
    ValueOfProduction <- calcOutput("VoP_crops",output="absolute",aggregate = FALSE)
    Production <- collapseNames(calcOutput("Production",products="kcr",aggregate=FALSE,attributes="dm"))
    years <- intersect(getYears(Production),getYears(ValueOfProduction))
    names <- intersect(getNames(Production),getNames(ValueOfProduction))
    Prices <- ValueOfProduction[,years,names] / Production[,years,names]
    
    missing<-setdiff(findset("kcr"),getNames(Prices))
    #Fill with maiz' value the missing crops (betr,begr,foddr)
    Prices<-add_columns(Prices,addnm = missing ,dim = 3.1)
    Prices[,,missing]<- Prices[,,"maiz"]
    Prices[!is.finite(Prices)] <- 0
    
    weight<-Production[,years,getNames(Prices)]
    weight[Prices==0]<-0
    
    
  } else {
   stop("Type not valid")
 } 
  
  }else if(products == "kli"){
  
    if (type== NULL){
  mappingFAO<-toolGetMapping("FAOitems.csv", type = "sectoral", where = "mappingfolder") #Reads mapping
  kli<-findset("kli")  
  items_intersect<-intersect(getNames(prices_prod_FAO),unique(mappingFAO$ProductionItem))# Kli items in mapping
  
  weight_p<-collapseNames(readSource("FAO","LivePrim")[,,"production"]) #Production of livestock primary prod.
  #subseting of items
  names<-intersect(getNames(weight_p),getNames(prices_prod_FAO[,,items_intersect]))
  years<-intersect(getYears(weight_p),getYears(prices_prod_FAO[,,items_intersect]))
  mappingFAO<-mappingFAO[mappingFAO$ProductionItem %in% names,]
    
  #Aggregation to magpie objects
  prices_prod_FAO_kli<-toolAggregate(prices_prod_FAO[,years,names],rel=mappingFAO,from="ProductionItem",to="k",weight=weight_p[,years,names],dim=3,wdim=3)
  
  #weight setup
  weight<-collapseNames(calcOutput("Production",products="kli",aggregate=FALSE,attributes="dm"))
  x<-prices_prod_FAO_kli
  
  years<-intersect(getYears(weight),getYears(x))
  names<-intersect(getNames(weight),getNames(x))
  
  weight <- weight[,years,names]
  x <- x[,years,names]
  
    }
    }else{
    stop("Type not valid")
  }
  
  units<-"USD/tDM"

  return(list(x=x,
              weight=weight,
              mixed_aggregation=NULL,
              unit=units,
              description="Value of production for individual crops"))
}
