#' Convert FRA 2020 data
#' 
#' @param x MAgPIE object containing original values
#' @param subtype The FAO FRA 2020 subtype.
#' @return Data as MAgPIE object with common country list
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}},
#' @examples
#' 
#' \dontrun{ a <- readSource("FRA2020","growing_stock",convert=TRUE)}
#' @importFrom madrat toolCountryFill
#' 

convertFRA2020 <- function(x,subtype){
  
  if(subtype == "growing_stock"){
    x <- toolCountryFill(x,fill = 0)
    out<- x/1000 ## Conversion from million m3 to billion m3
    return(out)
  } else {stop("Invalid subtype ", subtype)}
}
