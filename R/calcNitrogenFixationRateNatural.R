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
#' @importFrom mrlandcore toolLPJmLDefault

calcNitrogenFixationRateNatural <- function() {

  years <- findset("past_til2020")
  years <- as.integer(gsub("y", "", years))
  # evapotranspiration (in m^3 per ha)
  cfgLPJmL  <- mrlandcore::toolLPJmLDefault(suppressNote = FALSE)
  etRate <- calcOutput("LPJmLTransform",
                       lpjmlversion = cfgLPJmL$defaultLPJmLVersion,
                       climatetype  = cfgLPJmL$baselineGcm,
                       subtype      = "pnv:transp",
                       aggregate    = FALSE)
  cyears <- intersect(getYears(etRate, as.integer = TRUE), years)
  etRate <- etRate[, cyears, ]
  startYear <- "y1965"

  # HACKATHON - We need to figure out the yearly vs. monthly datasets
  etRate <- dimSums(etRate, dim = 3.1)

  landArea <- calcOutput("LandArea", aggregate = FALSE)
  et   <- etRate * landArea

  # calibration to global total of 58 Tg from Vitousek et al 2013,
  # assuming linear relation to evapotranspiration from Cleveland et al 1999
  bnf <- 58 / dimSums(setYears(et[, startYear, ], NULL), dim = c(1, 3)) * et
  getSets(bnf) <- c("x", "y", "iso", "year", "data")
  bnfRate                 <- bnf / landArea
  bnfRate[is.na(bnfRate)] <- 0

  # in case we also have ET for pasture, we could also first calibrate with natveg and
  # then apply to ET rates of pastures. however pasture productivity very uncertain


  return(list(x = bnfRate,
              weight = dimSums(landArea, dim = 3) + 10^-10,
              unit = "Mt Nr / Mha",
              description = "Nitrogen fixation freeliving bacteria",
              isocountries = FALSE))
}
