#' @title toolLPJmLVersion
#'
#' @description Specify default settings for LPJmL version and baseline settings
#'
#' @param version Switch between LPJmL versions (including add-ons (+*) for further version specification)
#' @param climatetype Switch between different climate scenarios
#'
#' @return configuration as list
#' @author Kristine Karstens
#'
#' @importFrom stringr str_split
#'
#' @export

toolLPJmLVersion <- function(version, climatetype) {
  cfg <- NULL

  ##### DEFAULT CONFIG #####
  cfg$baselineHist    <- "GSWP3-W5E5:historical"
  cfg$refYearHist     <- "y2010"
  cfg$baselineGcm     <- "MRI-ESM2-0:ssp370"
  cfg$refYearGcm      <- "y2020"
  cfg$readinVersion   <- version
  cfg$baselineVersion <- version
  cfg$climatetype     <- climatetype
  ##### DEFAULT CONFIG #####


  ##### ADDON CONFIG #####
  # overwrite default settings and LPJmL version for
  # (1) add-on tag in version argument - implemented add-ons:
  # * `+oldGSWP3`               - use older LPJmL version for GSWP3
  #                               as long as new GSWP3 is not available
  # * `+baselineGcm<GCM:RCP>`  - use another baseline for 2010--2020
  # * `+baselineHist<GCM:RCP>` - use another baseline for 1965--2010
  #
  # (2) add-on tag in climatetype argument - implemented add-ons:
  # * `_<scenarioAddon>   - add scenario specification to baseline climatetypes

  ### version addon
  if (grepl("\\+", version)) {
    tmp <- unlist(str_split(version, "\\+"))

    if (any(tmp == "oldGSWP3")) {
      if (climatetype == "GSWP3-W5E5:historical") {
        if (grepl("LPJmL4", tmp[1])) cfg$readinVersion <- "LPJmL4_for_MAgPIE_84a69edd"
        if (grepl("ggcmi", tmp[1]))  cfg$readinVersion <- "ggcmi_phase3_nchecks_fbed5c8b_newparam"
      } else {
        cfg$readinVersion <- tmp[1]
      }
    }

    if (any(grepl("baselineGcm", tmp))) {
      i <- grep("baselineGcm", tmp)
      cfg$baselineGcm   <- gsub("baselineGcm", "", tmp[i])
      cfg$readinVersion <- tmp[1]
    }

    if (any(grepl("baselineHist", tmp))) {
      i <- grep("baselineHist", tmp)
      cfg$baselineHist <- gsub("baselineHist", "", tmp[i])
      cfg$readinVersion   <- tmp[1]
    }

    ## Specific if in case the gsadapt scenario want to be harmonized to "standard" 2020
    # historical values and not with its own historical patterns

    if (any(grepl("gsadapt2020", tmp))) {

      if (cfg$climatetype != cfg$baselineHist) {
                cfg$readinVersion <- paste0(tmp[1], "+scen:gsadapt")
      } else {  cfg$readinVersion <- tmp[1] }

      cfg$baselineVersion <- tmp[1]
    }

  }
  ##### ADDON CONFIG #####

  return(cfg)
}
