#' @title readEDGAR8
#' @description read EDGAR 8 emission data
#' @return magpie objects of EDGAR historical emissions [MtCO2eq]
#' @seealso \code{\link{readSource}}
#' @author Pascal Weigmann
#' @importFrom readxl read_xlsx

readEDGAR8 <- function() {

  file <- "EDGARv8.0_FT2022_GHG_booklet_2023.xlsx"
  sheet <- "GHG_by_sector_and_country"

  # read in and clean source data and transform to magclass object
  # hard-coded last relevant row, so in case the data file changes -> adapt
  ed <- as.data.frame(read_xlsx(file, sheet = sheet, na = "NULL", n_max = 4778),
                      stringsAsFactors = FALSE) %>%
    rename(region = "EDGAR Country Code",
           variable = "Sector",
           pollutant = "Substance") %>%
    select(-c(Country)) %>%
    pivot_longer(cols = as.character(seq(1970,2022)),
                 names_to = "period") %>%
    as.magpie()

  return(ed)
}
