#' @title calcCroparea
#' @description Returns harvested areas of individual crops from FAOSTAT. Total harvested areas can be lower or higher than arable land because of multicropping or fallow land.
#' 
#' @param sectoral for landtype = "area_harvested" this aggregates FAO products. "ProductionItem" for unaggregated ProdSTAT items, "FoodBalanceItem" for Food Balance Sheet categories, "kcr" for MAgPIE items and "lpj" for LPJmL items.
#' @param physical if TRUE the sum over all crops agrees with the cropland area per country
#' @param cellular if TRUE crop area it calculates the cellular MAgPIE crop area for all magpie croptypes. Crop area from LUH2 crop types (c3ann, c4ann, c3per, c4per, cnfx) 
#' are mapped to MAgpIE crop types using mappingLUH2cropsToMAgPIEcrops.csv. Harvested areas of FAO weight area within a specific LUH crop type to devide into MAgPIE crop types.
#' @param cells Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param irrigation if true, the cellular areas are returned sperated by irrigated and rainfed. More about irrigation setup in calcLUH2v2 
#' @param years years to be estimated.
#'
#' @return areas of individual crops from FAOSTAT and weight
#' @author Ulrich Kreidenweis, Kristine Karstens, Felicitas Beier
#' @importFrom utils read.csv
#' @importFrom magclass fulldim setNames getCells collapseDim
#' @importFrom magpiesets findset addLocation
#' @importFrom madrat toolAggregate toolGetMapping

calcCroparea <- function(sectoral="kcr", physical=TRUE, cellular=FALSE, cells="magpiecell", irrigation=FALSE, years=NULL) {
  
  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit=1e+10)
  on.exit(options(magclass_sizeLimit=sizelimit))
  
  selectyears <- findset("past")
  
  if(!cellular){
    
    if(irrigation) stop("Irrigation levels for country based data not yet implemented!")
    
    #################################
    ### Croparea on country level ###
    #################################
    
    if (!is.null(sectoral) & !(sectoral=="lpj")) {
      CropPrim <- readSource("FAO_online", "Crop")[,,"area_harvested"]
      #use linear_interpolate
      Fodder   <- readSource("FAO", "Fodder")[,,"area_harvested"]
      Fodder   <- toolExtrapolateFodder(Fodder)
      data     <- toolFAOcombine(CropPrim, Fodder)/10^6 # convert to Mha
      
      if (sectoral%in%c("FoodBalanceItem","kcr")) {
        
        aggregation <- toolGetMapping("FAOitems_online.csv", type = "sectoral", where="mappingfolder")
        remove      <- setdiff(getNames(data, dim=1), aggregation$ProductionItem)
        data        <- data[,,remove, invert=TRUE]
        data        <- toolAggregate(data, rel=aggregation, from="ProductionItem", 
                                     to=ifelse(sectoral=="kcr","k", sectoral), dim=3.1, partrel=TRUE)
        if (sectoral=="kcr") {
          # add bioenergy with 0 values
          data <- add_columns(x = data,addnm = c("betr","begr"),dim = 3.1)
          data[,,c("betr","begr")] <- 0
          # remove all non kcr items
          kcr <- findset("kcr")
          remove <- setdiff(fulldim(data)[[2]][[3]],kcr)
          if(length(remove)>0){
            remain_area <- mean( dimSums(data[,,"remaining.area_harvested"], dim=1)/dimSums(dimSums(data[,,"area_harvested"], dim=3), dim=1) )
            if (remain_area > 0.02) vcat(1,"Aggregation created a 'remaining' category. The area harvested is", round(remain_area,digits = 3)*100, "% of total \n")
            vcat(2, paste0("Data for the following items removed: ", remove))
            data <- data[,,kcr]
          }
        }
      } else if(sectoral!="ProductionItem"){stop("Sectoral aggregation not supported")}
      
    } else if(sectoral=="lpj"){
      
      MAGcroparea <- calcOutput("Croparea", sectoral="kcr", physical=physical, cellular=FALSE, irrigation=FALSE, aggregate=FALSE)
      MAGtoLPJ    <- read.csv(toolMappingFile("sectoral","MAgPIE_LPJmL.csv"))
      MAGtoLPJ    <- MAGtoLPJ[!(MAGtoLPJ$MAgPIE=="pasture"),]
      LPJcroparea <- toolAggregate(MAGcroparea, rel=MAGtoLPJ, from="MAgPIE", to="LPJmL", dim=3.1)
      data        <- LPJcroparea
      
    } else {stop("Sectoral aggregation not supported")}
    
    # use the share of the single crops to calculate their "physical" area
    if (physical) {
      #6620  = (6620|Arable land and Permanent crops or  6620|Cropland)
      cropland        <- setNames(collapseNames(calcOutput("FAOLand", aggregate=FALSE)[,,"6620", pmatch=TRUE]), "crop") 
      harvested_share <- data/dimSums(data, dim=3.1)
      commonyears     <- intersect(getYears(cropland),getYears(harvested_share))
      data            <- collapseNames(cropland[,commonyears,]*harvested_share[,commonyears,])
    }
    
    data[is.na(data)] <- 0
    
  } else {
    
    ##################################
    ### Croparea on cellular level ###
    ##################################
    
    if (sectoral=="kcr") {
      
      #LUH related data input on cell level
      LUHweights       <- calcOutput("LUH2MAgPIE", share = "MAGofLUH", missing="fill", aggregate = FALSE) 
      LUHcroptypes     <- c("c3ann","c4ann","c3per","c4per","c3nfx")
      
      LUHcroparea      <- toolCell2isoCell(calcOutput("LUH2v2", landuse_types="LUH2v2", cells=cells, aggregate = FALSE, irrigation=irrigation, cellular=TRUE, selectyears="past"), cells=cells)
      
      LUHcroparea      <- LUHcroparea[,,LUHcroptypes]
      
      if (irrigation==TRUE) {
        LUHcroparea <- LUHcroparea[,,"total",invert=TRUE] #if "total" is also reported magpie object grows too big (>1.3GB)
      }
      
      LUH2MAG          <- LUHcroparea * toolIso2CellCountries(LUHweights, cells=cells)
      MAGcroparea      <- dimSums(LUH2MAG, dim=3.1)
      
      data             <- collapseNames(MAGcroparea)
      
    } else if(sectoral=="lpj") {
      
      MAGcroparea   <- calcOutput("Croparea", sectoral="kcr", physical=physical, cellular=TRUE, irrigation=irrigation, cells=cells, aggregate=FALSE)
      MAGtoLPJ      <- read.csv(toolMappingFile("sectoral","MAgPIE_LPJmL.csv"))
      MAGtoLPJ      <- MAGtoLPJ[!(MAGtoLPJ$MAgPIE=="pasture"),]
      LPJcroparea   <- toolAggregate(MAGcroparea, rel=MAGtoLPJ, from="MAgPIE", to="LPJmL", dim="MAG")
      data          <- LPJcroparea
      
      
    } else { stop("Not possible (for now) for the given item set (sectoral)!")}
    
    if (!physical) {
      
      MultiCropping <- calcOutput("Multicropping", selectyears = "past", aggregate = FALSE)
      
      data          <- data[,selectyears,] * toolIso2CellCountries(MultiCropping, cells=cells) 
      
    }
    
  }
  
  data <- collapseNames(data)
  
  return(list(x           = data,
              weight      = NULL, 
              unit        ="million ha", 
              description = "harvested crop areas from FAOSTAT",
              isocountries=!cellular))
}