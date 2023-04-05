#' @title toolClimateInputVersion
#'
#' @description Specify default settings for LPJmL climate input version and baseline settings
#' @param lpjmlVersion Add-ons (+*) for further version specification for LPJmL version
#' @param climatetype Switch between different climate scenarios
#'
#' @return configuration as list
#' @author Kristine Karstens
#'
#' @importFrom stringr str_split
#'
#' @export

toolClimateInputVersion <- function(lpjmlVersion, climatetype) {

  cfgLPJmL <- toolLPJmLVersion(lpjmlVersion, climatetype)
  cfg      <- NULL

  ##### DEFAULT CLIMATE CONFIG #####
  cfg$versionScen   <- "ISIMIP3bv2"
  cfg$versionHist   <- "ISIMIP3av2"
  cfg$baselineHist  <- cfgLPJmL$baseline_hist
  cfg$refYearHist   <- cfgLPJmL$ref_year_hist
  cfg$baselineGcm   <- cfgLPJmL$baseline_gcm
  cfg$refYearGcm    <- cfgLPJmL$ref_year_gcm
  cfg$climatetype   <- climatetype
  ##### DEFAULT  CLIMATE CONFIG #####

  if (cfg$climatetype == "GSWP3-W5E5:historical") {
    cfg$climatetype    <- "GSWP3-W5E5:obsclim"
  }

  return(cfg)
}
