#' @title convertISIMIPoutputs
#' @description convert data to ISO country level
#' 
#' @param x MAgPIE object on cellular level
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

convertISIMIPoutputs2 <- function(x){
  return(toolAggregateCell2Country(x, fill = 0))
}