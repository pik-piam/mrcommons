#' @importFrom dplyr filter_ full_join
readCEDS <- function(subtype) {
  files <- c(BC    = "CEDS_BC_emissions_by_country_CEDS_sector_v_10_25_2016.csv",
             CO    = "CEDS_CO_emissions_by_country_CEDS_sector_v_10_25_2016.csv",
             CH4   = "CEDS_CH4_emissions_by_country_CEDS_sector_v_04_30_2017.csv",
             N2O   = "CEDS_N2O_dummy.csv",
             NH3   = "CEDS_NH3_emissions_by_country_CEDS_sector_v_10_25_2016.csv",
             NMVOC = "CEDS_NMVOC_emissions_by_country_CEDS_sector_v_10_25_2016.csv",
             NOx   = "CEDS_NOx_emissions_by_country_CEDS_sector_v_10_25_2016.csv",
             OC    = "CEDS_OC_emissions_by_country_CEDS_sector_v_10_25_2016.csv",
             SO2   = "CEDS_SO2_emissions_by_country_CEDS_sector_v_10_25_2016.csv",
             CO2   = "CEDS_CO2_emissions_by_country_CEDS_sector_v_05_01_2017.csv")

  emi  <- read.csv(toolSubtypeSelect(subtype, files))

  emiLu <- read.csv("biomassburning_emissions_by_country_v1.0_ceds_country.csv")
  emiLu <- filter_(emiLu, ~ em == subtype)
  dimnames(emiLu)[[2]] <- gsub("X", "", dimnames(emiLu)[[2]])

  dimnames(emi)[[2]] <- gsub("X", "", dimnames(emi)[[2]])
  x <- full_join(emi, emiLu, by = names(emiLu))

  y <- as.magpie(x, spatial = 1, datacol = 5)
  getSets(y)[1] <- "ISO3"
  getSets(y)[2] <- "Year"

  return(y)
}
