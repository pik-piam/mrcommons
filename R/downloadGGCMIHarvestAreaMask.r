#' @title downloadGGCMIHarvestArea
#' @description Download ISIMIP GGCMI harvested area mask information for rice 1 and rice 2, as well as spring/winter wheat
#'
#' Downloads harvested area 
#'
#' @author David M Chen

downloadGGCMIHarvestAreaMask <- function(){

 
#spring and winter wheat
wheatfile = "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/landuse/winter_spring_wheat_separation/winter_and_spring_wheat_areas_phase3.nc4"

 if (file.exists(wheatfile)) {
          file.copy(wheatfile, basename(wheatfile))
         } else {
          vcat(1, paste0("Data for wheat masks could not be found!"))}

for (crop in c("swh", "wwh", "ri1", "ri2")){
    for (irr in c("ir", "rf")) {

path <- "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar"

 file <-  file.path(path, paste0(crop, "_", irr, "_ggcmi_crop_calendar_phase3_v1.01.nc4"))

      if (file.exists(file)) {
          file.copy(file, basename(file))
         } else {
          vcat(1, paste0("Data for requested subtype \"",path,"\" could not be found!"))}
    }
    }

  }




