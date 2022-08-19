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

#wheat 
wheatMasks <- rast("winter_and_spring_wheat_areas_phase3.nc4")
swh <- subset(wheatMasks, "swh_mask")
wwh <- subset(wheatMasks, "wwh_mask")

swh <- as.magpie(raster(swh))
wwh <- as.magpie(raster(wwh))

wheat <- mbind(swh, wwh)
getNames(wheat) <- gsub("_mask", "", getNames(wheat))
wheat <- add_dimension(wheat, dim = 3.2, add = "irr", nm = c("ir", "rf"))
#fill missing cells in wheat
  missing_cells <- setdiff(getItems(x, dim = 1), getItems(wheat, dim=1))
  fill <- new.magpie(cells_and_regions = missing_cells, years=getYears(wheat), names=getNames(wheat),  fill=0)
  wheat <- mbind(wheat, fill)

  x <- mbind(x, wheat)


x <- toolCoord2Isocell(x)

return(x)    }
