#' Read Eurostat historical emissions (env_air_gge)
#'
#' Read-in Eurostat historical emissions csv files as magclass object
#'
#' @param subtype 'emissions' for original Eurostat emissions split,
#' 'MACCemi' for MACC historical emissions, or 'sectorEmi' for sector specific
#' emissions, or 'latest' for most up-to-date data
#' @return magpie object of Eurostat historical emissions (MtCO2)
#' @author Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "Eurostat", subtype = "emissions")
#' }
#'
#' @importFrom madrat toolCountry2isocode
#' @importFrom reshape2 melt
#' @export

readEurostat <- function(subtype = "emissions") {
  switch(subtype,
    "emissions" = readEurostatEmissions(),
    "MACCemi" = readEurostatEmissions(),
    "sectorEmi" = readEurostatEmissions(),
    "latest" = readEurostatEmissionsLatest(),
    stop("Bad input for readEurostat. Invalid 'subtype' argument.")
  )
}


######################################################################################
# Functions
######################################################################################
# Reading Eurostat historical emissions from 2019
readEurostatEmissions <- function() {
  type <- c("GHG", "CO2", "CH4", "CH4_native", "N2O", "N2O_native", "HFC", "PFC", "HFC_PFC_NSP", "SF6", "NF3")
  data <- NULL
  for (t in type) {
    df <- read.csv(paste0("eurostat_", t, ".csv"))[, -c(3, 7)]
    colnames(df) <- c("period", "region", "emi", "sector", "value")
    df[df == ":"] <- NA
    df$value <- gsub(",", "", df$value)
    df$value <- as.numeric(df$value) / 1000 # convert from Thousand tonnes to Mt
    df$emi <- t
    data <- rbind(data, df)
  }
  # mapping reg
  data$region <- toolCountry2isocode(data$region,
    mapping = c("germany (until 1990 former territory of the frg)" = "DEU")
  )
  return(as.magpie(data, spatial = 2, temporal = 1, datacol = 5))
}

# Reading Eurostat latest historical emissions from 2024
readEurostatEmissionsLatest <- function() {

  # read in latest Eurostat data from 2024 ----
  df <- read.csv(file.path("2024", "env_air_gge_linear.csv")) %>%
    filter(.data$unit == "MIO_T", .data$geo != "EU27_2020") %>%
    select("region" = "geo", "period" = "TIME_PERIOD", "emi" = "airpol", "sector" = "src_crf", "value" = "OBS_VALUE")

  df$region <- toolCountry2isocode(df$region, mapping = c("EL" = "GRC"))

  x <- as.magpie(df, spatial = 1, temporal = 2, datacol = 5)
  getNames(x, dim = 1) <- c(
    "CH4_native", "CH4", "CO2", "GHG", "HFC", "HFC_PFC_NSP",
    "N2O_native", "N2O", "NF3", "PFC", "SF6"
  )

  sectorMap <- toolGetMapping("EurostatCRFLabels.csv", type = "sectoral", where = "mrcommons")
  sectorMap <- sectorMap[match(getNames(x, dim = 2), sectorMap$crf), ]
  getNames(x, dim = 2) <- sectorMap[, "label"]

  # read in GBR values from 2019 database ----
  gbr <- readEurostatEmissions()["GBR", , ]
  gbr <- add_columns(gbr, addnm = c("y2020", "y2021", "y2022"), dim = "period", fill = NA)

  # convert CH4 and N2O to use AR5 GWP values instead of AR4
  gbr[, , "CH4"] <- gbr[, , "CH4"] * 28 / 25
  gbr[, , "N2O"] <- gbr[, , "N2O"] * 265 / 298

  gbr <- magclass::matchDim(gbr, x, dim = 3)
  return(mbind(x, gbr))
}
