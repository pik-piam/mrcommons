#' Read Eurostat historical emissions
#'
#' Read-in Eurostat historical emissions csv files as magclass object
#'
#' @param subtype emissions for original eurostat emissions split, MACCemi for MACC historical emissions, or
#' sectorEmi for sector specific emissions
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
    stop("Bad input for readEurostat. Invalid 'subtype' argument.")
  )
}


######################################################################################
# Functions
######################################################################################
readEurostatEmissions <- function() {
  # Reading Eurostat historical emissions
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
