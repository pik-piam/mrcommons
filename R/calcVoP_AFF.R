#' @title calcVoP_AFF 
#' @description Calculates the overall value of production of the agriculture, forestry and fisheries sectors. Forestry and Fisheries are calculated from exports values.
#' 
#'
#'
#' @return magpie object. in mio. current USD units
#' @author Edna J. Molina Bacca
#' @importFrom dplyr intersect
#' @seealso \code{\link{calcOutput}}
#' @examples
#'
#' \dontrun{
#' a <- calcOutput("VoP_AFF")

#' }
#'
calcVoP_AFF <- function() {

# Value of production for Agriculture (crops and livestock)
  VoP_All<-readSource("FAO_online","ValueOfProd")
  VoP_Crops<- VoP_All[,,"2041|Crops (PIN).Gross_Production_Value_(constant_2014_2016_million_US$)_(USD)"]
  VoP_Livestock<-VoP_All[,,"2044|Livestock (PIN).Gross_Production_Value_(constant_2014_2016_million_US$)_(USD)"]

  VoP_agriculture<-VoP_Crops+VoP_Livestock #mio. USD15
  getNames(VoP_agriculture)<-"Agriculture"
  VoP_agriculture<-VoP_agriculture*(1+0.04)^5 #mio. current USD


# Value of production fisheries

  export_fish_value <- readSource("FishstatJ_FAO",subtype="exportsValue") # 1000 current USD, change once in madrat wprld
  export_fish_tonNet <- readSource("FishstatJ_FAO",subtype="exportsQuantity")#ton_net change once in madrat wprld
  
  #export_fish_value <-readFishstatJ_FAO(subtype="exportsValue")
  #export_fish_tonNet <- readFishstatJ_FAO(subtype="exportsQuantity")#ton_net change once in madrat wprld

  #production_fish_tonLive <- readSource("FishstatJ_FAO",subtype="Production")#ton liveNot really needed
  production_Aquatic_tonNet <- readSource("FAO","CBLive")[,,"2961|Aquatic Products, Other + (Total).production"]#ton net
  production_fishSea_tonNet <- readSource("FAO","CBLive")[,,"2960|Fish, Seafood + (Total).production"]#ton net

  production_fish_tonNet<-production_Aquatic_tonNet+production_fishSea_tonNet

  years_fish<-intersect(intersect(getYears(export_fish_value),getYears((export_fish_tonNet))),getYears(production_fish_tonNet))
  cells_fish<-intersect(intersect(getCells(export_fish_value),getCells((export_fish_tonNet))),getCells(production_fish_tonNet))

  VoP_fish<-export_fish_value[cells_fish,years_fish,]/export_fish_tonNet[cells_fish,years_fish,]*production_fish_tonNet[cells_fish,years_fish,]/1000 # mio. current USD
  VoP_fish[is.na(VoP_fish)]<-0
  VoP_fish[is.infinite(VoP_fish)]<-0
  getNames(VoP_fish)<-"Fisheries"


# Value of production forestry

  VoP_forestry_data <- readSource("FAO","ForestProdTrade")

  export_forestry_value <- VoP_forestry_data[,,"Roundwood.Export_Value_(Mio_US$)"] #mio. US
  getNames(export_forestry_value) <- "Roundwood"

  export_forestry_m3 <- VoP_forestry_data [,,"Roundwood.Export_Quantity_(m3)"] #m3
  getNames(export_forestry_m3) <- "Roundwood"

  production_forestry_m3 <- VoP_forestry_data [,,"Roundwood.Production_(m3)"] #m3
  getNames(export_forestry_m3) <- "Roundwood"

  years_forestry<-intersect(getYears(export_forestry_value),getYears((export_forestry_m3)))
  cells_forestry<-intersect(getCells(export_forestry_value),getCells((export_forestry_m3)))
  names_forestry<-intersect(getNames(export_forestry_value),getNames((export_forestry_m3)))

  price_forestry<-export_forestry_value[cells_forestry,years_forestry,]/export_forestry_m3[cells_forestry,years_forestry,]
  
  for (i in getYears(price_forestry,as.integer = TRUE)){
    price_forestry[,i,] <- price_forestry[,i,]*(1+0.04)^(2020-i) 
   }
  price_forestry[is.na(price_forestry)]<-0
  price_forestry[is.infinite(price_forestry)]<-0

  VoP_forestry<-toolCountryFill(x = production_forestry_m3*price_forestry,fill = 0) #mio. current USD
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

