#' @title readAndrijevic2019
#' @description read in governance index data from Andrijevic et al. 2019
#' @return governance index data at iso-country level
#'
#' @param subtype data used from governance2019 repository
#'
#' @author Felicitas Beier
#'
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("Andrijevic2019")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom magclass getSets as.magpie collapseNames new.magpie getYears getNames clean_magpie
#' @importFrom madrat toolCountryFill

readAndrijevic2019 <- function(subtype) {
  # read in data:
  file <- paste(subtype, "csv", sep = ".")
  data <- read.csv(file, header = TRUE, dec = ".", sep = ",")[, -1]

  # remove NAs:
  data <- data[!is.na(data$year), ]

  if (subtype == "master_proj_obs") {
    data <- data.frame(countrycode = data$countrycode,
                       scenario = data$scenario,
                       year = data$year,
                       governance = data$governance,
                       goveff = data$gov.eff,
                       corrcont = data$corr.cont,
                       readiness = data$readiness)
  }

  # format data for transformation to magpie object
  data$year <- paste0("y", data$year)

  # historical observed data
  histData <- data[data$scenario == "Observed", ]
  histData <- collapseNames(as.magpie(histData))
  histData <- toolCountryFill(histData, fill = NA)

  # projected scenario data
  projData <- data[data$scenario != "Observed", ]
  projData <- collapseNames(as.magpie(projData))
  projData <- toolCountryFill(projData, fill = NA)

  # merge historical and projected data into one object
  histData <- histData[, intersect(getYears(histData), getYears(projData)), , invert = TRUE]

  out <- new.magpie(cells_and_regions = magclass::getItems(histData, dim = 1),
                    years = c(getYears(histData), getYears(projData)),
                    names = getNames(projData),
                    fill = NA)
  out[, getYears(histData), ] <- histData
  out[, getYears(projData), ] <- projData

  getSets(out)[1] <- "iso"
  out             <- clean_magpie(out)

  return(out)
}
