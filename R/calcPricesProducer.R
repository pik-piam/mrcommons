#' @title calcPricesProducer
#' @description producer prices for agricultural products
#' 
#'
#' 
#'
#' @param products either "kcr" or "kcl"
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
calcPricesProducer <- function(products="kcr") {
  
  # Read Prices producer with FAO format
  prices_prod_FAO<-readSource("FAO_online","PricesProducerAnnual") #USD per ton 
  getNames(prices_prod_FAO)[getNames(prices_prod_FAO) == "257|Oil, palm"] <- "257|Oil palm"
  
  if (products == "kcr"){
  #items for aggregation
  mappingFAO<-toolGetMapping("FAO2LUH2MAG_croptypes.csv", type = "sectoral", where = "mrcommons")
  items_intersect<-intersect(getNames(prices_prod_FAO),unique(mappingFAO$ProductionItem))
  
  
  #weight: Production
  weight_p<-collapseNames(readSource("FAO_online","Crop")[,,"production"])
  getNames(weight_p)[getNames(weight_p) == "254|Oil palm fruit"] <- "257|Oil palm"
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
  
  
  }else if(products == "kli"){
  
  #  
  mappingFAO<-toolGetMapping("FAOitems.csv", type = "sectoral", where = "mrcommons") #Reads mapping
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

  return(list(x=x,
              weight=weight,
              mixed_aggregation=NULL,
              unit=units,
              description=" Value of production for individual crops"))
}
