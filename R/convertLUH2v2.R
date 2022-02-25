#' @importFrom magclass ncells setItems
#' @importFrom luscale groupAggregate

convertLUH2v2 <- function(x, subtype) {

  x       <- toolCoord2Isocell(x)
  mapping <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")
  
  x    <- setItems(x, dim = 1.1, value =  rep("GLO", ncells(x)))
  
  out  <- groupAggregate(data = x, query = mapping, 
                         from = "cell", to = "iso", dim = 1)
  out  <- toolCountryFill(out, fill = 0, verbosity = 2)
  
  return(out)
}
