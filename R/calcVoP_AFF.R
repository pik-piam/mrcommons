#' @title calcVoP_AFF 
#' @description Calculates the overall value of production of the agriculture, forestry and fisheries sectors. Forestry and Fisheries are calculated from exports values.
#' 
#'
#'
#' @return magpie object. in mio. current USD units
#' @author Edna J. Molina Bacca
#' @importFrom dplyr intersect
#' @importFrom magclass dimSums
#' @seealso \code{\link{calcOutput}}
#' @examples
#'
#' \dontrun{
#' a <- calcOutput("VoP_AFF")

#' }
#'
calcVoP_AFF <- function() {

#### Value of production for Agriculture (crops and livestock)
  Ag<-c("2041|Crops (PIN).Gross_Production_Value_(constant_2014_2016_million_US$)_(USD)",
        "2044|Livestock (PIN).Gross_Production_Value_(constant_2014_2016_million_US$)_(USD)")
 
   #factor converts from mio. 05 USD to current USD
  VoP_agriculture<-dimSums(readSource("FAO_online","ValueOfProd")[,,Ag],dim=3)*(1+0.04)^5 
  getNames(VoP_agriculture)<-"Agriculture"
  

#### Value of production fisheries
  
  #export value and quantity of fish and other aquatic products
  export_fish_value <- readSource("FishstatJ_FAO",subtype="exportsValue") # 1000 current USD
  export_fish_tonNet <- readSource("FishstatJ_FAO",subtype="exportsQuantity")#ton_net 
  
  Fish_Cat<-c("2961|Aquatic Products, Other + (Total).production",
              "2960|Fish, Seafood + (Total).production")
  
  production_fish_tonNet <- dimSums(readSource("FAO","CBLive")[,,Fish_Cat],dim=3.1)#ton net
  
  #common years
  years_fish<-intersect(intersect(getYears(export_fish_value),getYears((export_fish_tonNet))),getYears(production_fish_tonNet))
  cells_fish<-intersect(intersect(getCells(export_fish_value),getCells((export_fish_tonNet))),getCells(production_fish_tonNet))
  
  #Value of production for fish and aquatic products -> Production*export_price
  VoP_fish<-export_fish_value[cells_fish,years_fish,]/export_fish_tonNet[cells_fish,years_fish,]*production_fish_tonNet[cells_fish,years_fish,]/1000 # mio. current USD
  VoP_fish[!is.finite(VoP_fish)]<-0
  getNames(VoP_fish)<-"Fisheries"


#### Value of production forestry
  
  Forest_cat<-c("Roundwood.Export_Value_(Mio_US$)",
                "Roundwood.Export_Quantity_(m3)",
                "Roundwood.Production_(m3)")

  VoP_forestry_data <- readSource("FAO","ForestProdTrade")[,,Forest_cat]
  
  price_forestry<-VoP_forestry_data[,,"Roundwood.Export_Value_(Mio_US$)"]/VoP_forestry_data[,,"Roundwood.Export_Quantity_(m3)"]
  
  #to convert from each year money value to 2020
  for (i in getYears(price_forestry,as.integer = TRUE)){
    price_forestry[,i,] <- price_forestry[,i,]*(1+0.04)^(2020-i) 
  }
  
  price_forestry[!is.finite(price_forestry)]<-0

  VoP_forestry<-toolCountryFill(x = VoP_forestry_data[,,"Roundwood.Production_(m3)"]*price_forestry,fill = 0) #mio. current USD
  getNames(VoP_forestry)<-"Forestry"

################
  
  #magpie object to return
  years_VoP<-intersect(intersect(getYears(VoP_agriculture),getYears((VoP_fish))),getYears(VoP_forestry))
  cells_VoP<-intersect(intersect(getCells(VoP_agriculture),getCells((VoP_fish))),getCells(VoP_forestry))

  x<-mbind(VoP_agriculture[cells_VoP,years_VoP,],VoP_fish[cells_VoP,years_VoP,],VoP_forestry[cells_VoP,years_VoP,])
  
  
  return(list(x=x,
         weight=NULL,
         mixed_aggregation=NULL,
         unit="mio. current USD units",
         description=" Value of production for the agriculture, forestry and fisheries sector"))
}

