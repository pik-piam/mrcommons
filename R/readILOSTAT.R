#' @title readILOSTAT
#' @description Read in ILOSTAT data that has been downloaded from the ILOSTAT website
#'
#' @param subtype Type of ILOSTAT data that should be read
#' \itemize{
#' \item \code{EmplByActivityModelled}: "Employment by sex and economic activity -- ILO modelled estimates,
#' Nov. 2020 (thousands)"
#' \item \code{WeeklyHoursByActivity}: "Mean weekly hours actually worked per employed person by sex
#' and economic activity"
#' \item \code{HourlyLaborCostsByActivity}: "Mean nominal hourly labour cost per employee by economic activity"
#' \item \code{EmplByActivityMonthly}: "Employment by sex and economic activity (thousands) | Monthly"
#' \item \code{EmplByActivityMonthlyAdj}: "Employment by sex and economic activity, seasonally adjusted
#' series (thousands) | Monthly"
#' \item \code{EmplByActivityAndStatus}: "Employment by sex, status in employment and economic activity
#' (thousands) | Annual"
#' \item \code{WeeklyHoursByActivityMonthly}: "Mean weekly hours actually worked per employee by sex and economic
#'  activity | Monthly"
#' }
#' @return ILOSTAT data as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   readSource("ILOSTAT", "EmplByActivityModelled")
#' }
#' @importFrom utils read.table
#' @importFrom magclass as.magpie getSets
#' @importFrom stringr str_split
#' @importFrom dplyr arrange

readILOSTAT <- function(subtype) {

  # get indicator ID of dataset
  indicatorIDs <- c(
    EmplByActivityModelled       = "EMP_2EMP_SEX_ECO_NB_A",
    WeeklyHoursByActivity        = "HOW_TEMP_SEX_ECO_NB_A",
    HourlyLaborCostsByActivity   = "LAC_4HRL_ECO_CUR_NB_A",
    EmplByActivityMonthly        = "EMP_TEMP_SEX_ECO_NB_M",
    EmplByActivityMonthlyAdj     = "EMP_TEM1_SEX_ECO_NB_M",
    EmplByActivityAndStatus      = "EMP_TEMP_SEX_STE_ECO_NB_A",
    WeeklyHoursByActivityMonthly = "HOW_XEES_SEX_ECO_NB_M"
  )
  indicatorID <- toolSubtypeSelect(subtype, indicatorIDs)

  # read data
  ilo <- read.table(paste0(indicatorID, ".csv"), header = TRUE)

  # remove unreliable data
  if ("obs_status" %in% colnames(ilo)) {
    ilo <- ilo[!(ilo$obs_status %in% c("Unreliable", "Break in series", "Not significant")), ]
    ilo <- ilo[, setdiff(colnames(ilo), "obs_status")]
  }

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

  # split time dimension for monthly data
  if (length(grep("M", ilo$time)) > 0) {
    ilo$month <- as.integer(str_split(ilo$time, "M", simplify = T)[, 2])
    ilo$time <- paste0("y", str_split(ilo$time, "M", simplify = T)[, 1])
    ilo <- arrange(ilo, ilo$month)
    ilo$month <- month.abb[ilo$month]
    ilo <- ilo[, c(setdiff(colnames(ilo), "obs_value"), "obs_value")]
  }

  # transform into magclass object
  ilo <- magpiesort(as.magpie(ilo, temporal = "time", spatial = "ref_area"))
  getSets(ilo)[1:2] <- c("region", "year")

  return(ilo)
}
