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
  faoCleaning <- function(data, mapping, unitVar, value) {
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

    x <- magpiesort(as.magpie(data, temporal = 2, spatial = 1, datacol = 3))
    additionalWeights <- c(SVK = 2773, CZE = 25712, # CZE = production CSK 1992 minus SVK 1993
                           SXM = 1103.420, CUW = 3636.317, BES = 397.898, # gdp 2010
                           SSD = 37020, SDN = 41508) # production in 2012
    # note: confusingly SSD/South Sudan (which is a landlocked country) has a production
    # comparable to SDN/Sudan according to the data

    x <- toolISOhistorical(x, overwrite = TRUE, additional_weight = as.magpie(additionalWeights))
    x <- toolCountryFill(x = x, fill = 0) # fill with zeros
    getNames(x) <- value

    return(x)
  }

 # Cleaning based on output subtype selected
  if (subtype == "Production") {
    x <- faoCleaning(data = data, mapping = isocodeFAO, unitVar = "Tonnes - live weight", value = "Production")
  } else if (subtype == "exportsQuantity") {
    # actual unit is tonnes; unitVar is used to access the data
    x <- faoCleaning(data = data, mapping = isocodeFAO, unitVar = "Export", value = "exportsQuantity")
  } else if (subtype == "exportsValue") {
    # actual unit is 1000 current USD MER; unitVar is used to access the data
    x <- faoCleaning(data = data, mapping = isocodeFAO, unitVar = "Export", value = "exportsValue")
  }
  return(x)
}
