#' @title readGGCMICropCalendar
#' @description Reads in GGCMI fraction of Harvested Area masks for rice 1 and rice 2
#'  (other crops available too, see path in download fucntion), or other variables available in
#'  the GGCMI crop calendar.
#' @param subtype variable or vector of variables to read from the crop calendar set. Options:
#' ("planting_day","maturity_day","fraction_of_harvested_area")
#' @return MAgPIE object with the requested data
#' @author David M Chen, Edna Molina Bacca
#'
#' @importFrom raster raster
#' @importFrom terra rast subset
#'
readGGCMICropCalendar <- function(subtype="fraction_of_harvested_area"){

x <- NULL

    if(subtype == "fraction_of_harvested_area"){
       for (crop in c("ri1", "ri2")){
        for (irr in c("ir", "rf")) {

          mask <- rast(paste0(crop, "_", irr, "_ggcmi_crop_calendar_phase3_v1.01.nc4"))
          mask <- subset(mask, "fraction_of_harvested_area")
          mag <- as.magpie(raster(mask))
          getNames(mag) <- crop
          mag <- add_dimension(mag, dim = 3.2, add = "irr", nm = irr)
          mag <- add_dimension(mag, dim = 3.3, add = "var", nm = subtype)
          x <- mbind(x, mag)}}

          #Wheat
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
            wheat <- add_dimension(wheat, dim = 3.3, add = "var", nm = subtype)
            x <- mbind(x, wheat)

    }else{

       for (crop in c("ri1","ri2","soy","mai","swh","wwh")){
       for (irr in c("ir","rf")) {
       mask <- rast(paste0(crop, "_", irr, "_ggcmi_crop_calendar_phase3_v1.01.nc4"))
       mask <- subset(mask, subtype)
       mag <- as.magpie(raster(mask))
       getNames(mag) <- crop
       missing_cells <- setdiff(getItems(x, dim = 1), getItems(mag, dim=1))

      if(length(missing_cells)>0){
       fill <- new.magpie(cells_and_regions = missing_cells, years=getYears(mag), names=getNames(mag),  fill=NA)
       mag <- mbind(mag, fill)
       }
       mag <- add_dimension(mag, dim = 3.2, add = "irr", nm = irr)
       mag <- add_dimension(mag, dim = 3.3, add = "var", nm = subtype)
       x <- mbind(x, mag)

       }
  }

}


x <- toolCoord2Isocell(x)

return(x)    }
