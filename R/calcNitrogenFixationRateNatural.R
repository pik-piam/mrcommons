#' @title calcNitrogenFixationRateNatural
#' @description calculates fixation rates from natural ecosystems based on evapostranspiration
#' @return List of magpie objects with results on global level, empty weight, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcNitrogenFixationPast()]
#' [readHerridge()]
#' @examples
#' \dontrun{
#' calcOutput("NitrogenFixationRateNatural")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames dimSums setYears
#' @importFrom magpiesets findset

calcNitrogenFixationRateNatural <- function() {

  years <- findset("past")

  # evapotranspiration (in m^3 per ha)
  etRate    <- collapseNames(calcOutput("LPJmL_new", version = "LPJmL4_for_MAgPIE_44ac93de",
                                         climatetype = "GSWP3-W5E5:historical", subtype = "aet",
                                         stage = "smoothed", aggregate = FALSE)[, years, ])
  # reduce to 59199 cells and rename cells
  etRate    <- toolCoord2Isocell(etRate)
  startYear <- "y1965"

  land <- dimSums(setYears(calcOutput("LanduseInitialisation", aggregate = FALSE,
                                      cellular = TRUE)[, startYear, ], NULL),
                  dim = 3)
  et   <- etRate * land

  # calibration to global total of 58 Tg from Vitousek et al 2013,
  # assuming linear relation to evapotranspiration from Cleveland et al 1999
  bnf <- 58 / dimSums(setYears(et[, startYear, ], NULL), dim = c(1, 3)) * et
  bnfRate                  <- bnf / land
  bnfRate[is.na(bnfRate)] <- 0

  # in case we also have ET for pasture, we could also first calibrate with natveg and
  # then apply to ET rates of pastures. however pasture productivity very uncertain

  return(list(x = bnfRate,
              weight = dimSums(land, dim = 3),
              unit = "Mt Nr / Mha",
              description = "Nitrogen fixation  freeliving bacteria",
              isocountries = FALSE)
         )
}
