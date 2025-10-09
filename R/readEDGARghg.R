#' @title readEDGAR_GHG
#' @description read EDGAR GHG emission data
#' @param subtype ghg_by_sector to read in the sheet GHG_by_sector_and_country,
#'                lulucf to read in the sheet LULUCF_countries
#' @return magpie objects of EDGAR historical emissions (MtCO2eq)
#' @author Pascal Weigmann
#' @importFrom readxl read_xlsx

readEDGARghg <- function(subtype = "ghg_by_sector") {

  file <- "EDGAR_2025_GHG_booklet_2025.xlsx"

  if (subtype == "ghg_by_sector") {

    sheet <- "GHG_by_sector_and_country"

    # read in and clean source data and transform to magclass object
    # hard-coded last relevant row, so in case the data file changes -> adapt
    ed <- as.data.frame(read_xlsx(file, sheet = sheet, na = "NULL", n_max = 4798),
                        stringsAsFactors = FALSE) %>%
      rename(region = "EDGAR Country Code",
             variable = "Sector",
             pollutant = "Substance") %>%
      select(-c("Country")) %>%
      pivot_longer(cols = as.character(seq(1970, 2024)),
                   names_to = "period") %>%
      as.magpie()

  } else if (subtype == "lulucf") {

    sheet <- "LULUCF_countries"

    # read in and clean source data and transform to magclass object
    # hard-coded last relevant row, so in case the data file changes -> adapt
    # "Net" variable not read in as it seems to be just the sum of all others
    ed <- as.data.frame(read_xlsx(file, sheet = sheet, na = "NULL", n_max = 1067),
                        stringsAsFactors = FALSE) %>%
      rename(region = "EDGAR Country Code",
             variable = "Sector",
             pollutant = "Substance") %>%
      select(-c("Country", "Macro-region")) %>%
      pivot_longer(cols = as.character(seq(1990, 2024)),
                   names_to = "period") %>%
      as.magpie()

  } else {
    stop("Please choose the subtype `ghg_by_sector` or `lulucf`.")
  }

  return(ed)
}
