#' @title calcPlantEstablishCalib
#' @description 
#' Calculates the calibration factors for plantation establishment globally
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("PlantEstablishCalib",aggregate=TRUE)
#' }
#' @importFrom madrat toolMappingFile
#' @export

calcPlantEstablishCalib <- function(){
  
  ## Call mapping file
  mapping <- toolMappingFile(type = "regional",name = "h12.csv",readcsv = TRUE)
  mapping$value <- 1
  mapping[mapping$RegionCode == "EUR",]$value = 0.8
  mapping[mapping$RegionCode == "REF",]$value = 0.5
  mapping[mapping$RegionCode == "CAZ",]$value = 0.5
  mapping[mapping$RegionCode == "LAM",]$value = 4.0
  mapping[mapping$RegionCode == "OAS",]$value = 3.0
  mapping[mapping$RegionCode == "SSA",]$value = 1.2
  mapping[mapping$RegionCode == "MEA",]$value = 0.5

  weight <- x <- setNames(as.magpie(mapping[,c(-1,-3)],spatial = "CountryCode",temporal = NULL),NULL)
  weight[weight >= 0] <- 1
  
  return(list(x = x,
              weight = weight,
              min = 0,
              unit = "1",
              description = "Calibration factor for plantation establishment decisions"))
  
}
