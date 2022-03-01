#' @title downloadILOSTAT
#' @description Download data from ILOSTAT
#' @param subtype Type of ILOSTAT data that should be downloaded
#' @return metadata entry
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   downloadSource("ILOSTAT", "EmplByActivityModelled")
#' }
#' @importFrom dplyr select all_of
#' @importFrom stringr str_extract
#' @importFrom utils write.table

downloadILOSTAT <- function(subtype) {

  # get indicator ID of dataset
  indicatorIDs <- c(
    EmplByActivityModelled       = "EMP_2EMP_SEX_ECO_NB_A",
    WeeklyHoursByActivity        = "HOW_TEMP_SEX_ECO_NB_A",
    HourlyLaborCostsByActivity   = "LAC_4HRL_ECO_CUR_NB_A",
    EmplByISIC2                  = "EMP_TEMP_SEX_EC2_NB_A",
    EmplByActivityMonthly        = "EMP_TEMP_SEX_ECO_NB_M",
    EmplByActivityMonthlyAdj     = "EMP_TEM1_SEX_ECO_NB_M",
    EmplByActivityAndStatus      = "EMP_TEMP_SEX_STE_ECO_NB_A",
    WeeklyHoursByActivityMonthly = "HOW_XEES_SEX_ECO_NB_M",
    WeeklyHoursByISIC2           = "HOW_TEMP_SEX_EC2_NB_A",
    WeeklyHoursEmployeesBYISIC2  = "HOW_XEES_SEX_EC2_NB_A",
    LaborIncomeShareGDPModelled  = "LAP_2GDP_NOC_RT_A",
    OutputPerWorkerModelled      = "GDP_205U_NOC_NB_A"
  )
  indicatorID <- toolSubtypeSelect(subtype, indicatorIDs)

  # download and save data
  if (requireNamespace("Rilostat", quietly = TRUE)) {
    res <- Rilostat::get_ilostat(indicatorID, cache = FALSE)
  } else {
    stop("Rilostat is needed to download data from ILOSTAT")
  }
  remove <- intersect(c("source", "indicator", "note_indicator", "note_source", "note_classif"), colnames(res))
  res <- select(res, -all_of(remove))
  res[, -1] <- Rilostat::label_ilostat(res[, -1])
  write.table(res, paste0(indicatorID, ".csv"), row.names = FALSE)

  # get meta data
  toc <- Rilostat::get_ilostat_toc()
  toc <- toc[toc[, "id"] == indicatorID, ]
  unit <- gsub("[()]", "", str_extract(toc$indicator.label, "\\([^)]*\\)$"))
  if (is.na(unit)) unit <- ""
  url <- paste0("https://www.ilo.org/ilostat-files/WEB_bulk_download/indicator/", indicatorID, ".csv.gz")

  return(list(url           = url,
              doi           = "not available",
              title         = toc$id,
              author        = "International Labour Organization",
              version       = "not available",
              release_date  = toc$last.update,
              description   = toc$indicator.label,
              license       = "https://www.ilo.org/global/copyright/request-for-permission/lang--en/index.htm",
              unit          = unit,
              reference     = "not available")
  )
}
