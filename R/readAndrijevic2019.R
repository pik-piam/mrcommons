#' @title readAndrijevic2019
#' @description read in governance index data from Andrijevic et al. 2019
#' @return governance index data at iso-country resolution
#'
#' @param subtype data to be returned:
#'                "historical" for observed data until 2015
#'                "projected" for projected SSP scenario data from 2015 to 2099
#'
#' @author Felicitas Beier
#'
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource("Andrijevic2019")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom magclass getSets as.magpie collapseNames


readAndrijevic2019 <- function(subtype) {

  # read in data:
  file <- file.path("governance_obs_project/governance_obs_project.csv")
  data <- read.csv(file, header = TRUE, dec = ".", sep = ",")[, -1]

  # remove NAs:
  data <- data[!is.na(data$year), ]

  # format data for transformation to magpie object
  data$year <- paste0("y", data$year)

  if (subtype == "historical") {
    # historical observed data
    data <- data[data$scenario == "Observed", ]
    data <- collapseNames(as.magpie(data))
  } else if (subtype == "projected") {
    # projected scenario data
    data <- data[data$scenario != "Observed", ]
    data <- collapseNames(as.magpie(data))
  } else {
    stop("Please specify subtype: historical or projected")
  }

  getSets(data)[1] <- "iso"
  data             <- clean_magpie(data)

  return(data)
}
