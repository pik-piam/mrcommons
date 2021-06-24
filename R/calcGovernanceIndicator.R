#' @title calcGovernanceIndicator
#' @description returns governance indicator from Andrijevic et al. 2019
#' @return magpie object at iso-country level
#'
#' @importFrom magclass new.magpie getRegions getYears getNames
#' @importFrom madrat toolFillYears
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

  histData <- readSource("Andrijevic2019", subtype = "historical")
  projData <- readSource("Andrijevic2019", subtype = "projected")

  # merge historical and projected data into one object
  histData <- histData[, intersect(getYears(histData), getYears(projData)), , invert = T]

  out <- new.magpie(cells_and_regions = getRegions(histData),
                    years = c(getYears(histData), getYears(projData)),
                    names = getNames(projData),
                    fill = NA)
  out[, getYears(histData), ] <- histData
  out[, getYears(projData), ] <- projData

  # fill missing years
  out <- toolFillYears(out, years = c(getYears(out, as.integer = TRUE)[1]:2100))

  return(list(x            = out,
              weight       = NULL,
              unit         = "index",
              description  = "Governance Index in range from 0 to 1",
              isocountries = TRUE))

}
