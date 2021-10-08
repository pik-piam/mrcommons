#' @title convertExpertGuessSSPLivestockProduction
#' @description {convert the Expert Guesses for future Livestock Production for the SSP Scenarios
#' }
#' 
#' 
#' @param x MAgPIE-Object contaiing data to convert
#' 
#'
#' 
#' @return magpie object containing converted expert guesses
#' @author Stephen Wirth
#' @seealso [readSource()]
#' @examples
#' \dontrun{ 
#' a <- readSource("ExperGuessSSPLivestockProduction", "ssp1")

#' }


convertExpertGuessSSPLivestockProduction <- function(x){
  map <- toolGetMapping(type = "regional", name = "regionmappingMAgPIE.csv")
  y <- toolAggregate(x, map, from = 3, to = 2)
  return(y)
}
