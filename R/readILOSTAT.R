#' @title readILOSTAT
#' @description Read in ILOSTAT data that has been downloaded from the ILOSTAT website
#'
#' @param subtype Type of ILOSTAT data that should be read
#' \itemize{
#' \item \code{AgEmpl}: "Employment by sex and economic activity -- ILO modelled estimates, Nov. 2020 (thousands)"
#' \item \code{HourlyLaborCosts}: "Mean weekly hours actually worked per employed person by sex and economic activity"
#' \item \code{WeeklyHours}: "Mean nominal hourly labour cost per employee by economic activity"
#' }
#' @return ILOSTAT data as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   readSource("ILOSTAT", "AgEmpl")
#' }
#' @importFrom utils read.table
#' @importFrom magclass as.magpie getSets
#' @importFrom stringr str_split
#' @importFrom dplyr arrange_

readILOSTAT <- function(subtype) {

  # get indicator ID of dataset
  indicatorIDs <- c(
    AgEmpl            = "EMP_2EMP_SEX_ECO_NB_A",
    HourlyLaborCosts  = "LAC_4HRL_ECO_CUR_NB_A",
    WeeklyHours       = "HOW_TEMP_SEX_ECO_NB_A"
  )
  indicatorID <- toolSubtypeSelect(subtype, indicatorIDs)

  # read data
  ilo <- read.table(paste0(indicatorID, ".csv"), header = TRUE)

  # clean up descriptions
  for (n in seq(1, ncol(ilo))) {
    if (colnames(ilo)[n] %in% c("ref_area", "time", "obs_value")) next
    namesSplit <- str_split(ilo[, n], ": ", simplify = TRUE)
    if (length(unique(namesSplit[, 1])) == 1) {
      colnames(ilo)[n] <- gsub("[()]", "", gsub(" |-|, ", "_", tolower(namesSplit[1, 1])))
      ilo[, n] <- gsub("\\.", "", namesSplit[, 2])
    } else {
      categoriesSplit <- str_split(namesSplit[, 1], "( \\()|\\)", simplify = TRUE)
      if (length(unique(categoriesSplit[, 1])) > 1) stop("Different categories")
      colnames(ilo)[n] <- gsub(" |-", "_", tolower(unique(categoriesSplit[, 1])))
      ilo[, n] <- gsub("\\.", "", paste0(gsub("-", "_", categoriesSplit[, 2]), ": ", namesSplit[, 2]))
    }
  }
  if (length(grep("^X[0-9]", ilo[, "ref_area"])) > 0) ilo <- ilo[-grep("^X[0-9]", ilo[, "ref_area"]), ]

  # transform table to magclass object
  ilo <- arrange_(ilo, "time")
  ilo <- as.magpie(ilo, temporal = "time", spatial = "ref_area")
  getSets(ilo)[1:2] <- c("region", "year")

  return(ilo)
}
