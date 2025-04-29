#' @title readEDGAR_GHG
#' @description read EDGAR GHG emission data
#' @return magpie objects of EDGAR historical emissions (MtCO2eq)
#' @author Pascal Weigmann
#' @importFrom readxl read_xlsx

readEDGARghg <- function() {

  file <- "EDGAR_2024_GHG_booklet_2024.xlsx"
  sheet <- "GHG_by_sector_and_country"

  # read in and clean source data and transform to magclass object
  # hard-coded last relevant row, so in case the data file changes -> adapt
  ed <- as.data.frame(read_xlsx(file, sheet = sheet, na = "NULL", n_max = 4801),
                      stringsAsFactors = FALSE) %>%
    rename(region = "EDGAR Country Code",
           variable = "Sector",
           pollutant = "Substance") %>%
    select(-c("Country")) %>%
    pivot_longer(cols = as.character(seq(1970, 2023)),
                 names_to = "period") %>%
    as.magpie()

  return(ed)
}
