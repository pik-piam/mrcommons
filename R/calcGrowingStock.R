#' @title calcGrowingStock
#' @description 
#' Calculates the growing stocks on FAO data.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("GrowingStock",aggregate=TRUE)
#' }
#' @export

calcGrowingStock <- function(){
  
  ## Read Growing Stock
  out <- readSource("FRA2020",subtype = "growing_stock",convert = TRUE)
  
  return(list(x=out,
              weight=NULL,
              min=0,
              unit="Billion m3",
              description="Calculates Growing stocks as reported by Forest Resources Assessment Data 2020."))
  
}
