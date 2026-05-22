#' @title readCMIP7_CEDS
#'
#' @description reads in harmonized emission data used for CMIP7 from three files.
#' country_file for energy related emissions on country level and global bunker
#' emissions, global_file for F-Gases and AFOLU on global level,
#' cmip7-historical-ghg-eq for GHG totals.
#' Source: https://zenodo.org/records/15059443
#'
#' @return MAgPIE object
#' @author Pascal Weigmann

readCMIP7_CEDS <- function() {

  release <- "2025-12-07"

  files <- list.files(release)

  country_file <- files[grepl("country-history", files)]
  global_file <- files[grepl("global-workflow-history", files)]
  ghg_file <- files[grepl("cmip7-historical-ghg-eq", files)]

  country_data <- read.csv(paste0(release, "/", country_file))
  global_data <- read.csv(paste0(release, "/", global_file))
  ghg_data <- read.csv(paste0(release, "/", ghg_file))

  data <- tidyr::pivot_longer(rbind(country_data, global_data, ghg_data),
                              cols = paste0("X", as.character(seq(1750, 2023))),
                              names_to = "period",
                              values_to = "value")
  data <- data %>%
    mutate(period = gsub("X", "", .data$period)) %>%
    mutate(region = gsub("World|global", "GLO", .data$region)) %>%
    filter(.data$period >= 1990)

  mdata <- as.magpie(data, spatial = "region", temporal = "period")
  out <- clean_magpie(mdata)

  return(out)
}
