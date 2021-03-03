#' @title convertISIMIP
#' @description convert data to ISO country level
#' 
#' @param x MAgPIE object on cellular level
#' @param subtype data subtype 
#' 
#' @importFrom magclass collapseNames collapseDim dimSums
#' 
#' @return MAgPIE object on country level
#' 
#' @author Jan Philipp Dietrich, Felicitas Beier
#' @seealso \code{\link{readSource}}
#' 
#' @examples
#' \dontrun{
#' a <- readSource("ISIMIP", convert=TRUE)
#' }
#' 

convertISIMIP <- function(x, subtype){
  if (grepl("^airww", subtype)) {
    landarea <- collapseNames(dimSums(readSource("LUH2v2", subtype="states", convert="onlycorrect")[,"y1995",], dim=3))
    landarea <- collapseDim(landarea, dim="iso")
    weight   <- landarea
  } else stop("Aggregation rule for given subtype \"",subtype,"\" not defined!")
  return(toolAggregateCell2Country(x, weight = weight, fill = 0))
}