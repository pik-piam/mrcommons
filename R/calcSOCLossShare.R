#' @title calcSOCLossShare
#' @description Calculates soil organic carbon loss share on cellular level
#'
#' @param subsystems if FALSE just generic values will be used per climate zone ,
#'                   if TRUE crop specific values will be reported,
#'                   if aggregated crop specific factors will be aggregated using crop area
#' @param rate if change, change rates will be reported; if loss, loss rates will be reported
#' @param factor switch for different ipcc versions (ipccReduced, ipccReduced2019)
#' @param cells "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("SOCLossShare", aggregate = FALSE)
#' }
#'
calcSOCLossShare <- function(subsystems = FALSE, rate = "change", factor = "ipccReduced",
                             cells = "lpjcell") {

  ipccClimate        <- calcOutput("ClimateClass", aggregate = FALSE,
                                   datasource = factor)

  factor2SCF         <- c(ipccReduced     = "SCF_sub",
                          ipccReduced2019 = "SCF_sub2019")
  scf                <- toolSubtypeSelect(factor, factor2SCF)
  scfSub2IPCCclimate <- readSource("IPCC", subtype = scf, convert = FALSE)[, , getNames(ipccClimate)]

  if (subsystems == FALSE) {

    socLossShare           <- dimSums(ipccClimate * scfSub2IPCCclimate[, , "maiz"], dim = 3.1)
    getNames(socLossShare) <- "cshare"

  } else if (subsystems %in% c(TRUE, "aggregated")) {

    socLossShare           <- dimSums(ipccClimate * scfSub2IPCCclimate, dim = 3.1)

    if (subsystems == "aggregated") {
      magCrop      <- calcOutput("Croparea", physical = TRUE, cellular = TRUE,
                                 irrigation = FALSE, aggregate = FALSE)
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
                               cells = cells, years = "y1995", round = 6), dim = 3) + 10^-10

  return(list(x            = socLossShare,
              weight       = weight,
              unit         = "tC/tC",
              description  = "Soil organic carbon loss share per crop type",
              isocountries = FALSE))
}
