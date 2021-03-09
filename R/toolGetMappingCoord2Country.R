#' @title       toolGetMappingCoord2Country
#' @description loads mapping of cellular coordinate data (67420 halfdegree cells) to country iso codes
#' 
#' @param pretty If TRUE, coordinate data is returned as numeric 'lon' and 'lat' columns
#' 
#' @return data frame of mapping
#' 
#' @author Felicitas Beier, Kristine Karstens
#' 
#' @importFrom stringr str_split
#' 
#' @export

toolGetMappingCoord2Country <- function(pretty=FALSE){

  out <- readRDS(system.file("extdata/mapCoords2Country.rds", package = "mrcommons"))
  
  if(pretty){
    tmp <- gsub("p","\\.",str_split(out$coords,"\\.", simplify = TRUE))
    tmp <- as.data.frame(matrix(apply(tmp,2,as.numeric), dim(tmp)[1], dim(tmp)[2], 
                                dimnames=list(NULL, c("lon","lat"))),
                         stringsAsFactors=FALSE)
    out <- data.frame(out, tmp)
  }
  
  return(out)
}