#' @title toolGetMappingCoord2Country
#' @description loads mapping of cellular coordinate data (67420 halfdegree cells) to country iso codes
#' 
#' @return data frame of mapping
#' @author Felicitas Beier
#' @export

toolGetMappingCoord2Country <- function(){

  out <- readRDS(system.file("extdata/mapCoords2Country.rds", package = "mrcommons"))
  
  return(out)
}