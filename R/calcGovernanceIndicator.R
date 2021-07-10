#' @title calcGovernanceIndicator
#' @description returns governance indicator from Andrijevic et al. 2019
#' @return magpie object at iso-country level
#'
#' @importFrom madrat toolFillYears
#' @importFrom magclass getYears
#'
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("GovernanceIndicator", aggregate = FALSE)
#' }
#'
#' @export

calcGovernanceIndicator <- function() {

  x <- readSource("Andrijevic2019", subtype = "governance_obs_project")

  # fill missing years
  out <- toolFillYears(x, years = seq(1995,2150,by=5))

  pop <- calcOutput("Population",aggregate=FALSE)[,2010,1]
  getYears(pop) <- NULL
  getNames(pop) <- NULL
  
  return(list(x            = out,
              weight       = pop,
              unit         = "index",
              description  = "Governance Index in range from 0 to 1",
              isocountries = TRUE))

}
