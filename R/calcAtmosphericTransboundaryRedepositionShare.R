#' @title calcAtmosphericRedepositionShare
#' @description Calculates share of volatilised nitrogen emissions that is redeposited on different land types.
#' @param maxshare the maximum amount of emissions deposited within the same cell or country.
#' The remainder will be handled as global emission
#' @param scenario scenario
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("AtmosphericRedepositionShare")
#' }
#'
calcAtmosphericTransboundaryRedepositionShare <- function(maxshare = 0.8, scenario = "rcp45") {

  redepShare <- calcOutput("AtmosphericRedepositionShare", scenario = scenario, cellular = FALSE, aggregate = FALSE)
  dep <- calcOutput("AtmosphericDeposition", datasource = "ACCMIP",
                    glo_incl_oceans = FALSE, scenario = scenario, cellular = FALSE,
                    emission = FALSE, aggregate = FALSE)
  depGlo <- calcOutput("AtmosphericDeposition", datasource = "ACCMIP",
                       glo_incl_oceans = TRUE, scenario = scenario, cellular = TRUE,
                       emission = FALSE, aggregate = FALSE)
  emi <- calcOutput("AtmosphericDeposition", datasource = "ACCMIP",
                    glo_incl_oceans = FALSE, cellular = FALSE, emission = TRUE, aggregate = FALSE)
  emiGlo <- calcOutput("AtmosphericDeposition", datasource = "ACCMIP",
                       glo_incl_oceans = TRUE, cellular = TRUE, emission = TRUE, aggregate = FALSE)

  if (any(abs(depGlo - emiGlo) > 0.2)) {
    vcat(2, "Mismatch between global emissions and deposition in ACCMIP dataset")
  }

  land <- findset("land")
  localDeposition <- emi * redepShare[, , land]
  transboundaryDeposition <- collapseNames(dep - localDeposition)
  transboundaryEmissions <- collapseNames(emiGlo - dimSums(localDeposition, dim = c(1, 3.4)))

  transboundaryRedepositionShare <- transboundaryDeposition / transboundaryEmissions

  return(list(
    x = transboundaryRedepositionShare,
    weight = NULL,
    unit = "Mt NH3N and NO2N",
    description = paste("Share of emitted volatilised nitrogen that is redeposited",
                        "within the same country. Based on the assumption that most",
                        "of the N is redeposited in the same country.")))
}
