#' @title readVittis
#' @description Read-in cost data from Vittis dataset. 
#' @return National-scale costs of production for 10 crops, disaggregated in 9 distinct cost elements
#' @author Debbora Leip
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("Vittis")
#' }
#' @importFrom utils read.csv
#' @importFrom magclass getSets as.magpie complete_magpie


readVittis <- function() {
  
  file <- "global_agricultural_crop_costs_percountry.csv"
  data  <- read.csv(file, header = TRUE, dec = ".")[,-1]
  
  data$year <- "y2000"
  names(data)[1] <- c("CountryCode")
  names(data) <- gsub("_", ".", names(data))
  data <- as.magpie(data)
  getSets(data)[3] <- "CostElement.CropType"
  data <- complete_magpie(data)
  
  getNames(data, dim = 1) <- c("Infrastructure", "Machinery", "PlantProtection",
                               "Seeds", "Financing", "Fuel_Lubricant", "Fertilizer",
                               "Irrigation", "Labor")
  
  return(data)
}