#' @title convertGGCMIHarvestAreaMask
#' @description Reads in GGCMI fraction of Harvested Area masks for rice 1 and rice 2 
#'  (other crops available too, see path in download fucntion) 
#' @param x object coming from read function
#' @return MAgPIE object with the requested data
#' @author David M Chen
#' 
#' @importFrom raster raster
#' @importFrom terra rast subset
#' 
convertGGCMIHarvestAreaMask <- function(x){

t <- toolCoord2Isocell(x)

return(x)    }
