#' @title readCEDS2021
#'
#' @description reads in emission data from the CEDS database
#' @return MAgPIE object
#' @author Benjamin Leon Bodirsky, David Klein

readCEDS2021 <- function() {

  files <- c(BC    = "BC_CEDS_emissions_by_sector_country_2021_04_21.csv",
             CO    = "CO_CEDS_emissions_by_sector_country_2021_04_21.csv",
             CH4   = "CH4_CEDS_emissions_by_sector_country_2021_04_21.csv",
             N2O   = "N2O_CEDS_emissions_by_sector_country_2021_04_21.csv",
             NH3   = "NH3_CEDS_emissions_by_sector_country_2021_04_21.csv",
             NMVOC = "NMVOC_CEDS_emissions_by_sector_country_2021_04_21.csv",
             NOx   = "NOx_CEDS_emissions_by_sector_country_2021_04_21.csv",
             OC    = "OC_CEDS_emissions_by_sector_country_2021_04_21.csv",
             SO2   = "SO2_CEDS_emissions_by_sector_country_2021_04_21.csv",
             CO2   = "CO2_CEDS_emissions_by_sector_country_2021_04_21.csv")

  out <- NULL
  allyears <- paste0("y", 1750:2019)
  for (file_x in files) {

    emi  <- read.csv(file_x)
    dimnames(emi)[[2]] <- gsub("X", "y", dimnames(emi)[[2]])
    y <- as.magpie(emi, spatial = 2, datacol = 5)
    getSets(y)[1] <- "ISO3"
    getSets(y)[2] <- "Year"
    getNames(y, dim = 1) <- paste0(getNames(y, dim = 1), "_", getNames(y, dim = 3))
    y <- dimSums(y, dim = "units")
    missingYears <- setdiff(allyears, getYears(y))
    y <- time_interpolate(y, allyears)
    y[, missingYears, ] <- NA

    out <- mbind(out, y)
  }
  out <- clean_magpie(out)
  getSets(out) <- c("iso3", "year", "pollutant", "sector")
  out <- dimOrder(out, perm = c(2, 1))
  return(out)
}
