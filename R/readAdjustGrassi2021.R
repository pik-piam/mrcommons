#' @title readAdjustGrassi2021
#' @description Read in data from Grassi et al. 2021.
#' Adjustement factors for emission in GtCO2 yr-1.
#' @param subtype Either "data" or "weight"
#' @return Adjustement factors or weight as MAgPIE object
#' @author Michael Windisch, Florian Humpenoeder
#' @examples
#' \dontrun{
#' readSource("AdjustGrassi2021", subtype = "data")
#' }
#' @importFrom magclass as.magpie


readAdjustGrassi2021 <- function(subtype) {
  # Read adjustement factor data for available RCPs
  if (subtype == "data") {
    x <- NULL
    for (rcp in (c("RCP19", "RCP26", "RCP34", "RCP45", "RCP60", "RCPBU"))) {
      inter <- read.table(paste0("Grassi_et_al_2021_adjustment_factors_", rcp, ".csv"),
                          sep = ";",
                          header = TRUE,
                          check.names = FALSE)
      inter <- as.magpie(inter, spatial = 1)
      getNames(inter) <- rcp
      x <- mbind(x, inter)
    }
  }

  if (subtype == "weight") {
    wTable <- read.table("Grassi_et_al_2021_country_removals.csv", sep = ";", header = TRUE, check.names = FALSE)
    x <- as.magpie(wTable, spatial = 1)
  }

  return(x)
}
