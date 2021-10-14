#' @title readGGCMICropCalendar
#' @description Reads in planting, maturity, and harvest date based on GGCMI hybrid product of crop calendars 
#' @return A MAgPIE 0.5deg resolution, day of the year
#' @param subtype "cal" for the basic calendar "wheat_areas" for swh and wwh growing areas, "rice_areas" for 
#' @author David Chen
#' @importFrom raster brick rasterToPoints
#' @import magclass
#' @importFrom dplyr left_join


readGGCMICropCalendar <- function(subtype){

 mapping <- toolGetMapping(type="cell", name="CountryToCellMapping.csv")
  

  if (subtype == "cal"){
  
   crops <- c("bar", "bea", "cas", "cot", "mai", "mil",
           "nut","pea","pot","rap", "ri1", "ri2", "rye",
           "sgb", "sgc", "sor","soy", "sun", "swh","wwh")

   irri <- c("rf", "ir")

   dates <- c("planting_day", "maturity_day")

   read <- function(cr, ir, dat){
     # read in as magpie
     file <- paste0(cr,"_",ir,"_ggcmi_crop_calendar_phase3_v1.01.nc4")
     x         <- brick(file, var=dat)
     x         <- rasterToPoints(x)
     colnames(x)[c(1, 2)] <- c("lon", "lat")
     x         <- left_join(mapping, x, by = c("lat", "lon"), copy = TRUE)
     drop      <- c("^iso$|lpj|^cell$|lon|lat")
     x         <- as.magpie(x[, !grepl(drop, colnames(x))])
  
     getCells(x) <- gsub("_", ".", getCells(x))
     getItems(x,dim=3.1) <- cr
     getItems(x, dim=3.2) <- ir
     getItems(x, dim=3.3) <- dat  
     return(x)
     }

  out <- NULL
   for (cr in crops){
    for (ir in irri){
     for (dat in dates){
      tmp <- read(cr=cr, ir=ir, dat=dat)
      out <- mbind(tmp, out)      
    }}}

 ## some crops are left for some days after maturity until harvest, Elliott et al. 2015source data from pdf in source folder 
 days_to_harvest <- new.magpie(cells_and_regions = getItems(out,dim=1),
                              years = NULL,names = getItems(out, dim=3.1), fill = 0)
 days_to_harvest[,,c("mai", "soy")] <- 21
 days_to_harvest[,,c("swh", "wwh","bar", "rye", "rap")] <- 7

 out <- magclass::add_columns(out, addnm="harvest_day", dim=3.3, fill=0)
 out[,,"harvest_day"] <- out[,,"maturity_day"] + days_to_harvest

 }

if (subtype == "wheat_areas"){
  
  out <- NULL
  
   for (i in c("wwh", "swh")){
   
   tmp <- brick("winter_and_spring_wheat_areas_phase3.nc4", var=paste0(i, "_mask")) 
   tmp         <- rasterToPoints(tmp)
   colnames(tmp)[c(1, 2)] <- c("lon", "lat")
   tmp         <- left_join(mapping, tmp, by = c("lat", "lon"), copy = TRUE)
   drop      <- c("^iso$|lpj|^cell$|lon|lat")
   tmp         <- as.magpie(tmp[, !grepl(drop, colnames(tmp))])
   getItems(tmp, dim=3.1) <- i  
   
   out <- mbind(out,tmp)
   }

  }
 if (subtype == "rice_areas"){
   
   out <- NULL
   
   for (ri in c("ri1", "ri2")){
     
     tmp <- brick(paste0(ri,"_ir_ggcmi_crop_calendar_phase3_v1.01.nc4"), var="fraction_of_harvested_area") 
    
     tmp         <- rasterToPoints(tmp)
     colnames(tmp)[c(1, 2)] <- c("lon", "lat")
     tmp         <- left_join(mapping, tmp, by = c("lat", "lon"), copy = TRUE)
     drop      <- c("^iso$|lpj|^cell$|lon|lat")
     tmp         <- as.magpie(tmp[, !grepl(drop, colnames(tmp))])
     getItems(tmp, dim=3.1) <- ri  
     
     out <- mbind(out,tmp)
   }
   
 }
 
  return(out)
  
 }
