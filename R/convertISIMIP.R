#' @title convertISIMIP
#' @description convert data to ISO country level
#' 
#' @param x MAgPIE object on cellular level
#' @param subtype data subtype 
#' @return MAgPIE object on country level
#' 
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readSource}}
#' 
#' @examples
#' \dontrun{
#' a <- readSource("ISIMIPoutputs", convert=TRUE)
#' }
#' 

convertISIMIP <- function(x, subtype){
  if (grepl("^airww", subtype)) {
    weight <- toolSurfaceArea()
  } else stop("Aggregation rule for given subtype \"",subtype,"\" not defined!")
  return(toolAggregateCell2Country(x, weight = weight, fill = 0))
}