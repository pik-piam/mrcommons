#' @title readCEDS2025
#'
#' @description reads in emission data from the CEDS database
#'publication: https://zenodo.org/records/15059443
#' @return MAgPIE object
#' @author Pascal Weigmann

readCEDS2025 <- function() {

  files <- c(BC    = "BC_CEDS_estimates_by_country_sector_v_2025_03_18.csv",
             CO    = "CO_CEDS_estimates_by_country_sector_v_2025_03_18.csv",
             CH4   = "CH4_CEDS_estimates_by_country_sector_v_2025_03_18.csv",
             N2O   = "N2O_CEDS_estimates_by_country_sector_v_2025_03_18.csv",
             NH3   = "NH3_CEDS_estimates_by_country_sector_v_2025_03_18.csv",
             NMVOC = "NMVOC_CEDS_estimates_by_country_sector_v_2025_03_18.csv",
             NOx   = "NOx_CEDS_estimates_by_country_sector_v_2025_03_18.csv",
             OC    = "OC_CEDS_estimates_by_country_sector_v_2025_03_18.csv",
             SO2   = "SO2_CEDS_estimates_by_country_sector_v_2025_03_18.csv",
             CO2   = "CO2_CEDS_estimates_by_country_sector_v_2025_03_18.csv")

  out <- NULL
  allyears <- paste0("y", 1750:2023)
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
