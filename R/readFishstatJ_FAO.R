#' @title readFishstatJ_FAO
#' @description  Reads data of fisheries generated using the FishstatJ app of FAO.
#' Read-in specifically, exports_value, exports_quantity, and/or overall production of fish/aquatic products.
#'
#' @param subtype data subtype needed. Either "exportsValue", "exportsQuantity", or "Production"
#' @return magpie object of either tonnes of liveweight or 1000 current USD
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' a <- readSource("FishstatJ_FAO", "Production")
#' a <- readSource("FishstatJ_FAO", "exportsQuantity")
#' a <- readSource("FishstatJ_FAO", "exportsValue")
#' }
#' @seealso [readSource()]
#' @importFrom stats reshape
#' @importFrom madrat toolSubtypeSelect toolGetMapping toolISOhistorical toolCountryFill
#' @importFrom magclass magpiesort
readFishstatJ_FAO <- function(subtype = "Production") { # nolint
  # Files generated using the FishstatJ app
  subtypeFiles <- c(exportsValue = "FAOSTAT_data_1-26-2021_FishesTradeUSD.csv",
                    exportsQuantity = "FAOSTAT_data_1-26-2021_FishesTradeTonns.csv",
                    Production = "FAOSTAT_data_1-26-2021_FishesProduction.csv")

  # Subsetting based on type of requested output
  subtypeFile <- toolSubtypeSelect(subtype, subtypeFiles)
  isocodeFAO <- toolGetMapping("FAOiso_faocode.csv", where = "mrcommons")

  # Reads data
  data <- read.csv(system.file("extdata", "sectoral", subtypeFile, package = "mrcommons"))

  # Function to clean-up the data
  faoCleaning <- function(data = data, mapping = isocodeFAO,
                          subsetvar = "Unit..Name.", unitVar = "Tonnes - live weight", value = "Production") {
    yearsStats <- paste0("X.", 1984:2018, ".") # wide format
    data <- if (value == "Production") {
      data[, c("Country..Name.", "Unit..Name.", yearsStats)]
    } else if (value %in% c("exportsValue", "exportsQuantity")) {
      data[, c("Country..Name.", "Trade.flow..Name.", "Unit..Name.", yearsStats)]
    }
    yearsStats <- as.character(1984:2018)
    colnames(data) <- if (value == "Production") {
      c("Country", "Variable", yearsStats)
    } else if (value %in% c("exportsValue", "exportsQuantity")) {
      c("Country", "Variable", "Unit", yearsStats)
    }
    data <- data[data$Variable == unitVar, ] # read only "Tonnes - live weight","Export"
    data <- reshape(data, varying = yearsStats, direction = "long", idvar = c("Country", "Variable"),
                    v.names = "Value", timevar = "Year", times = yearsStats) # from wide to long format
    rownames(data) <- seq_len(nrow(data)) # fix names of rows
    data <- merge(data, mapping, by = "Country")
    data <- data[, c("ISO3", "Year", "Value")]
    data[, "Year"] <- as.numeric(data[, "Year"])

    # converts to magpie object tonnes - live weight, current 1000 USD
    x <- magpiesort(as.magpie(data, temporal = 2, spatial = 1, datacol = 3))
    # missing data for countries emerging from Czechoslovakia & Netherland Antilles (required for toolISOhistorical)
    additionalWeights <- c(CZE = 10331, # value for CSK 1992 minus SVK 1993
                           SXM = 35000, CUW = 152000, BES = 22000, # World Population Prospects: The 2019 Revision
                           SSD = 37020, SDN = 41508) # production for 2012
    x <- toolISOhistorical(x, overwrite = TRUE, additional_weight = as.magpie(additionalWeights))
    x <- toolCountryFill(x = x, fill = 0) # fill with zeros
    getNames(x) <- value

    return(x)
  }

 # Cleaning based on output subtype selected
  if (subtype == "Production") {
    x <- faoCleaning(data = data, mapping = isocodeFAO, subsetvar = "Unit..Name.",
                     unitVar = "Tonnes - live weight", value = "Production")
  } else if (subtype == "exportsQuantity") {
    x <- faoCleaning(data = data, mapping = isocodeFAO, subsetvar = "Trade.flow..Name.",
                     unitVar = "Export", value = "exportsQuantity")
  } else if (subtype == "exportsValue") {
    x <- faoCleaning(data = data, mapping = isocodeFAO, subsetvar = "Trade.flow..Name.",
                     unitVar = "Export", value = "exportsValue")
  }
  return(x)
}
