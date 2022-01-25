#' @title readIFPRIsubsidy
#' @description read subsidies for crops and livestock (and non-allocated) from IFPRI table
#'
#' @return magpie object of agricultural subsidies
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   readSource("IFPRIsubsidy")
#' }
#' @importFrom readxl read_excel
#'
readIFPRIsubsidy <- function() {

  # read data from excel table
  data <- read_excel("Support_Database_2021_version2.xlsx", "Aggregated_NRA")
  data <- data[, c("Country_Code", "Year", "CATPROD", "NRA_Cat", "Support_USD")]

  # subset to crop and livestock category
  data <- data[data$CATPROD %in% c("CRP", "LVS", "NAL"), ]
  data$CATPROD[data$CATPROD == "CRP"] <- "Crops"
  data$CATPROD[data$CATPROD == "LVS"] <- "Livestock"
  data$CATPROD[data$CATPROD == "NAL"] <- "Non-Allocated"

  # remove region aggregates (without country code)
  data <- data[data$Country_Code != "NA", ]

  # convert to magpie object
  data <- as.magpie(data, temporal = "Year", spatial = "Country_Code")

  return(data)
}
