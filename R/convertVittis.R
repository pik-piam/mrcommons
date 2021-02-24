#' @title convertVittis 
#' @description Aggregate production costs from Vittis dataset to MAgPIE crop 
#' categories and change unit from US$2000 to US$2005.
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing national-scale costs of production for 
#' 10 crops, disaggregated in 9 distinct cost elements
#' @author Debbora Leip

convertVittis <- function(x) {
  
  # fill countries
  x <- toolCountryFill(x)
  
  # map to MAgPIE categories with global crop areas as weights
  mapping <- toolGetMapping("VittisCropCategories.csv", type = "sectoral", where = "mrcommons")
  weights <- toolAggregate(calcOutput("Croparea", sectoral = "ProductionItem", aggregate = "GLO")[,"y2000", mapping[,"ProductionItem"]], mapping, from = "ProductionItem", to = "Vittis", dim = 3)
  x <- toolAggregate(x, mapping, weight = weights, from = "Vittis", to = "kcr", dim = 3.2)
  
  # convert US$2000 to US$2005
  ppp_current <- readSource("WDI", subtype = "NY.GDP.MKTP.PP.CD")
  ppp_2011 <- readSource("WDI", "NY.GDP.MKTP.PP.KD") # 2011 ppp dataset
  
  PPPratio2011to2005 <- setYears(ppp_current["USA", "y2005", ]/ppp_2011["USA", "y2005", ],NULL) 
  PPPratio2011to2000 <- setYears(ppp_current["USA", "y2000", ]/ppp_2011["USA", "y2000", ],NULL) 
  PPPratio2000to2005 <- collapseDim(PPPratio2011to2005/PPPratio2011to2000, dim = 3)

  x <- x * PPPratio2000to2005
  
  return(x)
}