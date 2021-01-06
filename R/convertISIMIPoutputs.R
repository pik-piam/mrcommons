#' @title convertISIMIPoutputs
#' @description convert data to ISO country level
#' 
#' @param x MAgPIE object on cellular level
#' @return MAgPIE object on country level
#' 
#' @author Felicitas Beier
#' @seealso \code{\link{readSource}}
#' 
#' @examples
#' \dontrun{
#' a <- readSource("ISIMIPoutputs", convert=TRUE)
#' }
#' 
#' @importFrom madrat toolCountryFill toolMappingFile
#' @importFrom mrcommons toolCell2isoCell

convertISIMIPoutputs <- function(x){
  
  # Adjust cell names
  x <- toolCell2isoCell(x)

  # Aggregate to country data
  map <- toolMappingFile(type="cell", readcsv=T, name="CountryToCellMapping.csv")
  out <- toolAggregate(x, rel=map, from=2, to=3, partrel=T)
  # island states are NAs: will be set to 0
  out <- toolCountryFill(out,fill=0)
  
  return(out)
}