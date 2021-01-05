#' @title calcGrowingStockNatVegAbsolute
#' @description 
#' Calculates the growing stocks on FAO data.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @examples
#' \dontrun{ 
#' calcOutput("GrowingStockNatVegAbsolute",aggregate=TRUE)
#' }
#' 
#' @importFrom magpiesets FRAnames
#' @export

calcGrowingStockNatVegAbsolute <- function(){
  
  ## Read Growing Stock
  out <- readSource("FRA2020",subtype = "growing_stock",convert = TRUE)
  x <- out[,,grep(pattern = "tot",x = getNames(out),value = TRUE)]
  getNames(x) <- gsub(pattern = "gs_tot_",replacement = "",x = getNames(x))
  getNames(x) <- FRAnames(getNames(x))
  
  x <- setYears(collapseNames(x[,"y2000","Natural Forest"]),NULL)
  
  return(list(x=x,
              weight=NULL,
              min=0,
              unit="Mm3",
              description="Calculates Growing stocks as reported by Forest Resources Assessment Data 2020."))
  
}
