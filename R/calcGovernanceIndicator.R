#' @title calcGovernanceIndicator
#' @description returns governance indicator from Andrijevic et al. 2019
#' @return magpie object at iso-country level
#'
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("GovernanceIndicator", aggregate = FALSE)
#' }
#'
#' @export
#'
calcGovernanceIndicator <- function() {

  x <- readSource("Andrijevic2019", subtype = "governance_obs_project")

  # fill missing years
  out <- toolFillYears(x, years = seq(1995, 2150, by = 5))

  pop <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[, 2010, ]
  getYears(pop) <- NULL
  getNames(pop) <- NULL

  list(x            = out,
       weight       = pop,
       unit         = "index",
       description  = "Governance Index in range from 0 to 1",
       isocountries = TRUE)
}
