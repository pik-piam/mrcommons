#' @title readGGCMIHarvestAreaMask
#' @description Reads in GGCMI fraction of Harvested Area masks for rice 1 and rice 2 
#'  (other crops available too, see path in download fucntion) 
#' @return MAgPIE object with the requested data
#' @author David M Chen
#' 
#' @importFrom raster raster
#' @importFrom terra rast subset
#' 
readGGCMIHarvestAreaMask <- function(){

x <- NULL

for (crop in c("ri1", "ri2")){
    for (irr in c("ir", "rf")) {
    
mask <- rast(paste0(crop, "_", irr, "_ggcmi_crop_calendar_phase3_v1.01.nc4"))
mask <- subset(mask, "fraction_of_harvested_area")
mag <- as.magpie(raster(mask))

getNames(mag) <- crop
mag <- add_dimension(mag, dim = 3.2, add = "irr", nm = irr)

x <- mbind(x, mag)}}

return(x)    }
