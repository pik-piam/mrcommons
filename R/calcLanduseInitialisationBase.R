#' @title calcLanduseInitialisationBase
#' @description Calculates the cellular MAgPIE landuse initialisation area. Data from FAO on forestry is used
#' to split the secondary forest pool of the LU2v2 dataset into forestry and secd_forest. This function
#' returns the data set in a basic configuration. Use \code{\link{calcLanduseInitialisation}} for
#' more settings.
#'
#' @param cells "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @param selectyears Years to be computed (default on "past")
#' @return Cellular landuse initialisation in its base configuration
#' @author Jan Philipp Dietrich, Benjamin Leon Bodirsky, Kristine Karstens, Felcitas Beier, Patrick v. Jeetze
#' @examples
#' \dontrun{
#' calcOutput("LanduseInitialisationBase")
#' }


calcLanduseInitialisationBase <- function(cells = "magpiecell", selectyears = "past") {
  selectyears <- sort(findset(selectyears, noset = "original"))

  luTargetCountry <- calcOutput("LanduseInitialisation", aggregate = FALSE, nclasses = "nine", fao_corr = TRUE,
                                selectyears = selectyears, cellular = FALSE, cells = cells)
  luInit <- calcOutput("LanduseInitialisation", aggregate = FALSE, nclasses = "nine", fao_corr = FALSE,
                       selectyears = selectyears, cellular = TRUE, cells = cells)

  vegC  <- calcOutput("LPJmL_new", version = "LPJmL4_for_MAgPIE_44ac93de", climatetype = "GSWP3-W5E5:historical",
                      subtype = "vegc", stage = "smoothed", aggregate = FALSE)[, getYears(luTargetCountry), ]
  vegC <- toolCoord2Isocell(vegC, cells = cells)


  out <- toolFAOForestRelocate(luInit = luInit, luTargetCountry = luTargetCountry, vegC = vegC)
  if(any(out<0)) {
    out[out < 0] <- 0
    vcat(0,"Negativ land values detected and replaced by 0.")
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    min = 0,
    max = 14900, ### global land area
    description = "Land use initialisation data for different land pools",
    isocountries = FALSE
  ))
}
