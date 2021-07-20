#' @title downloadILOSTAT
#' @description Download data from ILOSTAT
#'
#' @param subtype Type of ILOSTAT data that should be downloaded
#' \itemize{
#' \item \code{AgEmpl}: "Employment by sex and economic activity -- ILO modelled estimates, Nov. 2020 (thousands)"
#' \item \code{HourlyLaborCosts}: "Mean weekly hours actually worked per employed person by sex and economic activity"
#' \item \code{WeeklyHours}: "Mean nominal hourly labour cost per employee by economic activity"
#' }
#' @return metadata entry
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   downloadSource("ILOSTAT", "AgEmpl")
#' }
#' @importFrom Rilostat get_ilostat label_ilostat get_ilostat_toc
#' @importFrom dplyr select all_of
#' @importFrom stringr str_extract
#' @importFrom utils write.table

downloadILOSTAT <- function(subtype) {

  # get indicator ID of dataset
  indicatorIDs <- c(
    AgEmpl            = "EMP_2EMP_SEX_ECO_NB_A",
    HourlyLaborCosts  = "LAC_4HRL_ECO_CUR_NB_A",
    WeeklyHours       = "HOW_TEMP_SEX_ECO_NB_A"
  )
  indicatorID <- toolSubtypeSelect(subtype, indicatorIDs)

  # download and save data
  res <- get_ilostat(indicatorID, cache = FALSE)
  remove <- intersect(c("source", "indicator", "obs_status", "note_indicator", "note_source", "note_classif"),
                      colnames(res))
  res <- select(res, -all_of(remove))
  res[, -1] <- label_ilostat(res[, -1])
  write.table(res, paste0(indicatorID, ".csv"), row.names = FALSE)

  # get meta data
  toc <- get_ilostat_toc()
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
              license       = "not available",
              unit          = unit,
              reference     = "not available")
  )
}
