#' @title calcPlantEstablishCalib
#' @description
#' Calculates the calibration factors for plantation establishment globally
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @examples
#' \dontrun{
#' calcOutput("PlantEstablishCalib", aggregate = TRUE)
#' }
#' @importFrom madrat toolGetMapping
#' @export

calcPlantEstablishCalib <- function() {
  ## Call mapping file
  mapping <- toolGetMapping(type = "regional", name = "h12.csv", where = "mappingfolder")
  mapping$value <- 1
  mapping[mapping$RegionCode == "LAM", ]$value <- 2.0
  mapping[mapping$RegionCode == "OAS", ]$value <- 1.5
  mapping[mapping$RegionCode == "SSA", ]$value <- 1
  mapping[mapping$RegionCode == "EUR", ]$value <- 1
  mapping[mapping$RegionCode == "NEU", ]$value <- 1
  mapping[mapping$RegionCode == "MEA", ]$value <- 0.3
  mapping[mapping$RegionCode == "REF", ]$value <- 3
  mapping[mapping$RegionCode == "CAZ", ]$value <- 1
  mapping[mapping$RegionCode == "CHA", ]$value <- 1
  mapping[mapping$RegionCode == "IND", ]$value <- 1.5
  mapping[mapping$RegionCode == "JPN", ]$value <- 1
  mapping[mapping$RegionCode == "USA", ]$value <- 1

  weight <- x <- setNames(as.magpie(mapping[, c(-1, -3)], spatial = "CountryCode", temporal = NULL), NULL)
  weight[weight >= 0] <- 1

  return(list(x = x,
              weight = weight,
              min = 0,
              unit = "1",
              description = "Calibration factor for plantation establishment decisions"))

}
