#' @title readHourlyLaborCostsChina
#' @description Read in data on hourly labor costs in agriculture for China.
#'
#' @return data as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' readSource("HourlyLaborCostsChina")
#' }

readHourlyLaborCostsChina <- function() {

  hourlyCostsChina <- read.magpie("China_hourly_laborcost_magpie.csv")

  return(hourlyCostsChina)
}
