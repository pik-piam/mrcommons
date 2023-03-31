#' @title calcSOCLossShare
#' @description Calculates soil organic carbon loss share on cellular level
#'
#' @param subsystems if FALSE just generic values will be used per climate zone ,
#'                   if TRUE crop specific values will be reported,
#'                   if aggregated crop specific factors will be aggregated using crop area
#' @param rate if change, change rates will be reported; if loss, loss rates will be reported
#' @param ipcc switch for different ipcc versions
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("SOCLossShare", aggregate = FALSE)
#' }
#'
calcSOCLossShare <- function(subsystems = FALSE, rate = "change", ipcc = "guide2006") {

  years                  <- findset("past")
  kgClimate             <- readSource("Koeppen", subtype = "cellular", convert = "onlycorrect")[, years, ]
  kgIPCC                <- toolGetMapping("mapping_koeppen_ipcc.csv", type = "sectoral")
  getNames(kgClimate)   <- tolower(getNames(kgClimate))
  kgIPCC$koeppen_geiger <- tolower(kgIPCC$koeppen_geiger)
  year2climateClasses    <- c(guide2006 = "ipcc_reduced", guide2019 = "ipcc_reduced2019")
  climateClasses         <- toolSubtypeSelect(ipcc, year2climateClasses)

  ipccClimate           <- toolAggregate(kgClimate, rel = kgIPCC, from = "koeppen_geiger", to = climateClasses, dim = 3)

  year2SCF               <- c(guide2006 = "SCF_sub", guide2019 = "SCF_sub2019")
  scf                    <- toolSubtypeSelect(ipcc, year2SCF)
  scfSub2IPCCclimate     <- readSource("IPCC",    subtype = scf, convert = FALSE)[, , getNames(ipccClimate)]

  if (subsystems == FALSE) {

    socLossShare           <- dimSums(ipccClimate * scfSub2IPCCclimate[, , "maiz"], dim = 3.1)
    getNames(socLossShare) <- "cshare"

  } else if (subsystems %in% c(TRUE, "aggregated")) {

    socLossShare           <- dimSums(ipccClimate * scfSub2IPCCclimate, dim = 3.1)

    if (subsystems == "aggregated") {
      magCrop      <- calcOutput("Croparea", physical = TRUE, cellular = TRUE, irrigation = FALSE, aggregate = FALSE)
      kcr2all      <- data.frame(list(kcr = getNames(socLossShare), all = rep("all", 19)))
      socLossShare <- toolAggregate(socLossShare, weight = magCrop, rel = kcr2all, from = "kcr", to = "all", dim = 3)
      getNames(socLossShare) <- "cshare"
    }
  }

  if (rate == "loss") {
    socLossShare <- 1 - socLossShare
  } else if (rate != "change") {
    stop(paste(rate, "is unknown as 'rate' parameter specification."))
  }

  weight <- dimSums(calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE,
                               input_magpie = FALSE, years = "y1995", round = 6), dim = 3)

  return(list(
    x            = socLossShare,
    weight       = weight,
    unit         = "tC/tC",
    description  = "Soil organic carbon loss share per crop type",
    isocountries = FALSE))
}
