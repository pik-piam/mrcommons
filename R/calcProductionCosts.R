#' @title calcProductionCosts
#' @description calculates agricultural production costs (split into different cost categories)     
#' @param datasource Datasource of production costs, currently only "Vittis"
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ProductionCosts", source = "Vittis")
#' }
#' 

calcProductionCosts <- function(datasource = "Vittis") {
  if (datasource == "Vittis") {
    costs <- readSource("Vittis")                                        # US$05/ha
    areas <- calcOutput("Croparea", sectoral = "kcr", aggregate = FALSE) # "million ha"
    
    # converting to absolute costs
    costs <- costs * areas[, getYears(costs), getNames(costs, dim = 2)]

  } else {
    stop("Source not available")
  }
  return(list(x = costs,
              weight = NULL,
              unit = "million US$05/yr",
              description = "production costs"))
}
  
  
