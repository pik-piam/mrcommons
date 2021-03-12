#' @title calcFAOharmonized
#' @description Calculate harmonized FAO Commodity Balance and Food Supply data based on CB, only harvested areas are taken from ProdSTAT. 
#'              This functions adds the CBCrop, CBLive, FSCrop and FSLive data together.
#' 
#' @return FAO harmonized data, weight as NULL, and a description as as a list of MAgPIE objects
#' @author Ulrich Kreidenweis, David Chen, Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' a <- calcOutput("FAOharmonized")
#' 
#' }
#' 
#' @importFrom magclass fulldim

calcFAOharmonized <- function () {
  

  # input data: Commodity Balance (Crops Primary + Livestock Primary), Food Supply (Crops Primary + Livestock Primary)
  CBCrop <- readSource("FAO_online", "CBCrop")
  CBLive <- readSource("FAO_online", "CBLive")
  FSCrop <- readSource("FAO_online", "FSCrop")
  FSLive <- readSource("FAO_online", "FSLive")
  
  CB <- toolFAOcombine(CBLive,CBCrop, combine="Item")
  FS <- toolFAOcombine(FSLive,FSCrop, combine="Item")
  rm(CBCrop, CBLive, FSCrop, FSLive)
  
  FAOdata <- mbind(CB, FS)
  
   ## in addition harvested area from Crops Primary

  Prod <- readSource("FAO_online", "Crop", convert=TRUE)

  ## aggregate Prod to CB units

  aggregation <- toolGetMapping("FAOitems_online.csv", type = "sectoral", where="mappingfolder")
  #remove  aggregate categories   
  remove <- setdiff(getNames(Prod, dim=1), aggregation$ProductionItem)
  Prod <- Prod[,,remove,invert=T]
  
  area_harvested <- toolAggregate(Prod, rel=aggregation, from="ProductionItem", to="FoodBalanceItem", dim=3.1, partrel=T)[,,"area_harvested"]

  # commonitems <- intersect(getNames(area_harvested, fulldim=T)[[1]],getNames(FAOdata, fulldim=T)[[1]])
  commonyears <- intersect(getYears(area_harvested), getYears(FAOdata))
  
  # FAOdata <- mbind(FAOdata[,commonyears,commonitems], area_harvested[,commonyears,paste(commonitems,"area_harvested", sep=".")])
  
  FAOdata <- mbind(FAOdata[,commonyears,], area_harvested[,commonyears,])
  
  rm(area_harvested)
  
  
  ### add Fodder data ###
  
  Fodder <- readSource("FAO", "Fodder")
  Fodder <- add_columns(x = Fodder,addnm = "domestic_supply",dim = 3.2)
  Fodder[,,"domestic_supply"]<-Fodder[,,"feed"]
  Fodderaggregated <- toolAggregate(Fodder, rel=aggregation, from="ProductionItem", 
                                    to="FoodBalanceItem", dim=3.1, partrel=T)

newyears <- setdiff(getYears(FAOdata),getYears(Fodderaggregated))
  
Fodderaggregated <- toolHoldConstant(Fodderaggregated, years=newyears)  

FAOdata <- mbind(FAOdata, Fodderaggregated)
  
  rm(Fodder, Fodderaggregated); gc()
  
  FAOdata[is.na(FAOdata)] <- 0

  ## check if there is data without an element name
  
  ## what to do? In case there is data these dimensions should not be deleted
  
  if(any(fulldim(FAOdata)[[2]][[3]]=="")) {
    if (sum(FAOdata[,,""]) == 0) {
      FAOdata <- FAOdata[,,"", invert=T]
    } else  {
      vcat(1,'Aggregation created entries without name (""), but containing information. This should not be the case.')
    }
  }
  
  if(any(getNames(FAOdata)=="remaining.production")) {
    remain_prod <- mean( dimSums(FAOdata[,,"remaining.production"], dim=1)/dimSums(dimSums(FAOdata[,,"production"], dim=3), dim=1) )
    if (remain_prod > 0.02) vcat(1,"Aggregation created a 'remaining' category. Production is", round(remain_prod,digits = 3)*100, "% of total \n")
  }
  if(any(getNames(FAOdata)=="remaining.area_harvested")) {
    remain_area <- mean( dimSums(FAOdata[,,"remaining.area_harvested"], dim=1)/dimSums(dimSums(FAOdata[,,"area_harvested"], dim=3), dim=1) )
    if (remain_area > 0.02) vcat(1,"Aggregation created a 'remaining' category. The area harvested is", round(remain_area,digits = 3)*100, "% of total \n")
  }
  # FAOdata <- FAOdata[,,"remaining", invert=T] 
  
  
  ## conversion from tonnes to Mt, hectares to Mha and 10^6kcal to 10^12kcal.
  FAOdata <- FAOdata/10^6
              
  return(list(x=FAOdata,weight=NULL, description="FAO Commodity Balance and Food Supply data", unit="Unit in Mt/yr, for area Mha, calories in 10^12 kcal/yr", note="food_supply_kcal, protein_supply and fat_supply were calculated from per capita per day values"))
}
