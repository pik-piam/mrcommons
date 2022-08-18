#' @title downloadGGCMIHarvestArea
#' Download ISIMIP GGCMI harvested area mask information for rice 1 and rice 2, as well as spring/winter wheat
#'
#' Downloads harvested area 
#'
#' @author David M Chen

downloadGGCMIHarvestAreaMask <- function(){

path <- "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar"

for (crop in c("swh", "wwh", "ri1", "ri2")){
    for (irr in c("ir", "rf")) {
    
 file <-  file.path(path, paste0(crop, "_", irr, "_ggcmi_crop_calendar_phase3_v1.01.nc4"))

      if (file.exists(file)) {
          file.copy(file, basename(file))
         } else {
          vcat(1, paste0("Data for requested subtype \"",path,"\" could not be found!"))}
    }
    }

  }




