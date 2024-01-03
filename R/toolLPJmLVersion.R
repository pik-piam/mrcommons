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
  #  * `+baseline_gcm<GCM:RCP>`  - use another baseline for 2010--2020
  #  * `+scen:<scenname>` - implemented scenario will be handled automatically for
  #                         the readLPJmL call, no additional changes needed here
  #  * `gsadapt2020`      - Specific if in case the gsadapt scenario want to be
  #                         harmonized to "standard" 2020 historical values and
  #                         not with its own historical patterns

  ### version addon
  if (grepl("\\+", version)) {
    tmp <- unlist(str_split(version, "\\+"))

    if (any(grepl("baseline_gcm", tmp))) {
      i <- grep("baseline_gcm", tmp)
      cfg$baseline_gcm   <- gsub("baseline_gcm", "", tmp[i])
      cfg$readin_version <- tmp[1]
    }

    if (any(grepl("gsadapt2020", tmp))) {

      if (cfg$climatetype != cfg$baseline_hist) {
        cfg$readin_version <- paste0(tmp[1], "+scen:gsadapt")
      } else {
        cfg$readin_version <- tmp[1]
      }

      cfg$baseline_version <- tmp[1]
    }
  }
  ##### ADDON CONFIG #####

  return(cfg)
}
