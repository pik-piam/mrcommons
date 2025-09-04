#' @title calcIPCCefNSoil
#' @description
#' Emission factors for croplands based on IPCC using a country-level leaching fraction
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("IPCCefNSoil")
#' }
#'
calcIPCCefNSoil <- function() {

  efnSoil <- setYears(readSource("IPCC", "efnsoil", convert = FALSE), NULL)
  leach <- calcOutput("IPCCfracLeach", round = 4, cellular = FALSE, aggregate = FALSE)
  surplus <- collapseNames(calcOutput("NitrogenBudgetCropland", aggregate = FALSE)[, , "surplus"])
  efnSoil <- (surplus * 0 + 1) * efnSoil
  efnSoil[, , c("inorg_fert", "man_crop", "resid", "som")][, , "no3_n"] <- leach[, , "crop"]

  return(list(x = efnSoil,
              weight = surplus,
              unit = "Share",
              description = paste("Emission factors from cropland soils. If IPCC, using",
                                  "the ipcc emission factors as share of applied N inputs.",
                                  "If Nloss, as share of cropland budget surplus.")))
}
