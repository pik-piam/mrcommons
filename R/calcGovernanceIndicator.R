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
  out <- toolFillYears(x, years = c(getYears(x, as.integer = TRUE)[1]:2100))

  return(list(x            = out,
              weight       = NULL,
              unit         = "index",
              description  = "Governance Index in range from 0 to 1",
              isocountries = TRUE))

}
