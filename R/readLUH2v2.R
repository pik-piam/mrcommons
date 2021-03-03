#' @title readLUH2v2
#' @description read LUH inputs
#' 
#' @param subtype switch between different inputs
#' 
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Florian Humpenoeder, Stephen Wirth, Kristine Karstens, Felicitas Beier, Jan Philipp Dietrich
#'
#' @importFrom ncdf4 nc_open
#' @importFrom raster raster extent brick subset aggregate projectRaster extent<- as.matrix extract
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom magclass as.magpie mbind
#' @importFrom madrat getConfig
#' @importFrom stringr str_match

readLUH2v2 <- function(subtype) {
  
  # basic settings  
  time_sel   <- seq(1901,2015,by=1)
  offset     <- 849  #year 850=1, year 1900=1051, year 2015=1166
  # grep years to set other than default years, if subtypes ends with '_850to1901' like time span expression
  if(all(!is.na(time_span <- str_match(subtype, "_(\\d+)to(\\d+)")[2:3]))){
    time_sel  <- seq(time_span[1],time_span[2],by=1)
    subtype   <- gsub("_(\\d+)to(\\d+)","",subtype)
  } 
  
  #File to process
  f_states <- "states.nc"
  f_man    <- "management.nc"
  
  ### Define dimensions
  map       <- toolGetMappingCoord2Country(pretty=TRUE)
  
  if(grepl("states", subtype)){
    
    #Open file and process information
    nc_file <- nc_open(f_states)
    data <- setdiff(names(nc_file$var),c("secma","secmb","lat_bounds","lon_bounds"))
    #Land area
    carea <- raster("staticData_quarterdeg.nc",varname="carea")
    extent(carea) <- c(-180,180,-90,90)
    
    x  <- NULL
    for(item in data){
      shr <- subset(brick(f_states,varname=item),time_sel-offset)
      mag <- aggregate(shr*carea,fact=2,fun=sum)
      mag <- as.magpie(extract(mag,map[c("lon","lat")]),spatial=1,temporal=2)
      getNames(mag) <- item
      getCells(mag) <- paste(map$coords,map$iso,sep=".")
      getYears(mag) <- time_sel
      getSets(mag)  <- c("x.y.iso","t","data")
      x <- mbind(x,mag)
    }
    
    #Convert from km^2 to Mha
    x <- x/10000
    
  } else if (grepl("irrigation", subtype)) {
    
    #Mapping between states and management_irrigation
    data_man    <- c("irrig_c3ann","irrig_c3per","irrig_c4ann","irrig_c4per","irrig_c3nfx","flood")
    data_states <- c("c3ann","c3per","c4ann","c4per","c3nfx","c3ann")
    data        <- matrix(data=c(data_man,data_states),ncol=2)
    
    #Land area
    carea         <- raster("staticData_quarterdeg.nc",varname="carea")
    extent(carea) <- c(-180,180,-90,90)
    
    x  <- NULL
    for(item in data){
      shr    <- subset(brick(f_states, varname=data[data[,1]==item,2]), time_sel-offset)
      ir_shr <- subset(brick(f_man,    varname=item)                  , time_sel-offset)
      #grid cell fraction of crop area x grid cell area x irrigated fraction of crop area
      mag <- aggregate(shr*carea*ir_shr,fact=2,fun=sum)
      mag <- as.magpie(extract(mag,map[c("lon","lat")]),spatial=1,temporal=2)
      getNames(mag) <- item
      getYears(mag) <- time_sel
      getCells(mag) <- paste(map$coords,map$iso,sep=".")
      getSets(mag)  <- c("x.y.iso","t","data")
      x <- mbind(x,mag)
    }
    
    #Convert from km^2 to Mha
    x <- x/10000
    
  } else if (grepl("ccode", subtype)) {
    
    #Load raster data on 0.25° and extend to full grid
    ccode25         <- raster("staticData_quarterdeg.nc",varname="ccode")
    extent(ccode25) <- c(-180,180,-90,90)
    
    #Create new raster object on 0.5° and re-project 0.25°-raster on 0.5°-raster
    r50     <- raster(res=0.5)
    ccode50 <- projectRaster(ccode25,r50,over=TRUE, method="ngb") #re-project to regular grid
    
    x <- as.magpie(extract(ccode50,map[c("lon","lat")]),spatial=1)
    getYears(x) <- 2000
    getNames(x) <- "ccode"
    getCells(x) <- paste(map$coords,map$iso,sep=".")
    getSets(x)  <- c("x.y.iso","t","ccode")
  }
  
  return(clean_magpie(x))
}

