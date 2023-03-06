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
  cfg$baseline_hist    <- "GSWP3-W5E5:historical"
  cfg$ref_year_hist    <- "y2010"
  cfg$baseline_gcm     <- "MRI-ESM2-0:ssp370"
  cfg$ref_year_gcm     <- "y2020"
  cfg$readin_version   <- version
  cfg$baseline_version <- version
  cfg$climatetype      <- climatetype
  ##### DEFAULT CONFIG #####


  ##### ADDON CONFIG #####
  # overwrite default settings and LPJmL version for
  # (1) add-on tag in version argument - implemented add-ons:
  # * `+oldGSWP3`               - use older LPJmL version for GSWP3
  #                               as long as new GSWP3 is not available
  # * `+baseline_gcm<GCM:RCP>`  - use another baseline for 2010--2020
  # * `+baseline_hist<GCM:RCP>` - use another baseline for 1965--2010
  #
  # (2) add-on tag in climatetype argument - implemented add-ons:
  # * `_<scenario_addon>   - add scenario specification to baseline climatetypes

  ### version addon
  if (grepl("\\+", version)) {
    tmp <- unlist(str_split(version, "\\+"))

    if (any(tmp == "oldGSWP3")) {
      if (climatetype == "GSWP3-W5E5:historical") {
        if (grepl("LPJmL4", tmp[1])) cfg$readin_version <- "LPJmL4_for_MAgPIE_84a69edd"
        if (grepl("ggcmi", tmp[1]))  cfg$readin_version <- "ggcmi_phase3_nchecks_fbed5c8b_newparam"
      } else {
        cfg$readin_version <- tmp[1]
      }
    }

    if (any(grepl("baseline_gcm", tmp))) {
      i <- grep("baseline_gcm", tmp)
      cfg$baseline_gcm   <- gsub("baseline_gcm", "", tmp[i])
      cfg$readin_version <- tmp[1]
    }

    if (any(grepl("baseline_hist", tmp))) {
      i <- grep("baseline_hist", tmp)
      cfg$baseline_hist  <- gsub("baseline_hist", "", tmp[i])
      cfg$readin_version <- tmp[1]
    }

    ## Specific if in case the gsadapt scenario want to be harmonized to "standard" 2020
    # historical values and not with its own historical patterns

    if (any(grepl("gsadapt2020", tmp))) {

      if (cfg$climatetype != cfg$baseline_hist) {
                cfg$readin_version <- paste0(tmp[1], "+scen:gsadapt")
      } else {  cfg$readin_version <- tmp[1] }

      cfg$baseline_version <- tmp[1]
    }

  }
  ##### ADDON CONFIG #####

  return(cfg)
}
