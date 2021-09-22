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
  cfg$baseline_hist <- "GSWP3-W5E5:historical"
  cfg$ref_year_hist <- "y2010"
  cfg$baseline_gcm  <- "MRI-ESM2-0:ssp370"
  cfg$ref_year_gcm  <- "y2020"
  cfg$lpjml_version <- version
  cfg$climatetype   <- climatetype
  ##### DEFAULT CONFIG #####


  ##### ADDON CONFIG #####
  # overwrite default settings and LPJmL version for add-on tag in version argument
  # implemented add-ons:
  # * `+oldGSWP3`               - use older LPJmL version for GSWP3
  #                               as long as new GSWP3 is not available
  # * `+baseline_gcm<GCM:RCP>`  - use another baseline for 2010--2020
  # * `+baseline_hist<GCM:RCP>` - use another baseline for 1965--2010
  # * `+scen:<scenario_addon>   - add scenario specification to cimatetype variable

  if (grepl("\\+", version)) {

    tmp     <- unlist(str_split(version, "\\+"))

    if (any(tmp == "oldGSWP3") & climatetype == "GSWP3-W5E5:historical") {

      if (grepl("LPJmL4", tmp[1])) cfg$lpjml_version <- "LPJmL4_for_MAgPIE_84a69edd"
      if (grepl("ggcmi", tmp[1])) cfg$lpjml_version <- "ggcmi_phase3_nchecks_fbed5c8b_newparam"

    } else {

      cfg$lpjml_version       <- tmp[1]
    }

    if (any(grepl("baseline_gcm", tmp))) {

      i <- grep("baseline_gcm", tmp)
      cfg$baseline_gcm  <- gsub("baseline_gcm", "", tmp[i])
    }

    if (any(grepl("baseline_hist", tmp))) {

      i <- grep("baseline_hist", tmp)
      cfg$baseline_hist <- gsub("baseline_hist", "", tmp[i])
    }

    if (any(grepl("scen", tmp))) {

      scen  <- toolSplitSubtype(tmp[grep("scen", tmp)],
                                list(prefix = "scen", scen = NULL))$scen

      cfg$baseline_hist <- paste("GSWP3-W5E5:historical", scen, sep = "_")
      cfg$baseline_gcm  <- paste("MRI-ESM2-0:ssp370",     scen, sep = "_")
      cfg$climatetype   <- paste(climatetype,             scen, sep = "_")
      cfg$lpjml_version <- tmp[1]

    }
  }
  ##### ADDON CONFIG #####

  return(cfg)
}
