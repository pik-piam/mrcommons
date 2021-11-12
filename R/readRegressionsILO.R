#' @title readRegressionsILO
#' @description Read regression coefficients which are used to fill missing values of ILO datasets
#' @param subtype Type of ILOSTAT data for which regression coefficients should be read
#' \itemize{
#' \item `AgEmpl`: "Employment by sex and economic activity -- ILO modelled estimates, Nov. 2020 (thousands)"
#' \item `HourlyLaborCosts`: "Mean nominal hourly labour cost per employee by economic activity"
#' }
#' @return regression coefficients as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   readSource("RegressionsILO", "AgEmpl")
#' }
#' @importFrom magclass read.magpie

readRegressionsILO <- function(subtype = "AgEmpl") {
  res <- read.magpie(paste0(subtype, ".csv"))
  return(res)
}
