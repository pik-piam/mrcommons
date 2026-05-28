#' @title readCMIP7_CEDS
#'
#' @description reads in harmonized emission data used for CMIP7 from three files.
#' countryFile for energy related emissions on country level and global bunker
#' emissions, globalFile for F-Gases and AFOLU on global level,
#' cmip7-historical-ghg-eq for GHG totals.
#' Source: https://zenodo.org/records/15059443
#'
#' @return MAgPIE object
#' @author Pascal Weigmann

readCMIP7_CEDS <- function() {  # nolint

  release <-  "2025-12-07"

  files <- list.files(release)

  countryFile <- files[grepl("country-history", files)]
  globalFile <- files[grepl("global-workflow-history", files)]
  ghgFile <- files[grepl("cmip7-historical-ghg-eq", files)]

  countryData <- read.csv(paste0(release, "/", countryFile))
  globalData <- read.csv(paste0(release, "/", globalFile))
  ghgData <- read.csv(paste0(release, "/", ghgFile))

  data <- tidyr::pivot_longer(rbind(countryData, globalData, ghgData),
                              cols = paste0("X", as.character(seq(1750, 2023))),
                              names_to = "period",
                              values_to = "value")
  data <- data |>
    mutate(period = gsub("X", "", .data$period)) |>
    mutate(region = gsub("World|global", "GLO", .data$region)) |>
    filter(.data$period >= 1990)

  mdata <- as.magpie(data, spatial = "region", temporal = "period")
  out <- clean_magpie(mdata)

  return(out)
}
