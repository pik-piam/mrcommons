#' @title readFishstatJ_FAO
#' @description  Reads data of fisheries generated using the FishstatJ app of FAO.
#' Read-in specifically, exports_value, exports_quantity, and/or overall production of fish/aquatic products.
#'
#'
#' @param subtype data subtype needed. Either "exportsValue", "exportsQuantity", or "Production"
#' @return magpie object of either tonnes of liveweight or 1000 current USD
#' @author Edna J. Molina Bacca
#' @importFrom stats reshape
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("FishstatJ_FAO", "Production")
#' a <- readSource("FishstatJ_FAO", "exportsQuantity")
#' a <- readSource("FishstatJ_FAO", "exportsValue")
#' }
#'
readFishstatJ_FAO <- function(subtype = "Production") { # nolint: object_name_linter.
  # Files generated using the FishstatJ app
  files <- c(exportsValue       = "FAOSTAT_data_1-26-2021_FishesTradeUSD.csv",
             exportsQuantity    = "FAOSTAT_data_1-26-2021_FishesTradeTonns.csv",
             Production         = "FAOSTAT_data_1-26-2021_FishesProduction.csv")

  # Subsetting based on type of requested output
  file <- toolSubtypeSelect(subtype, files)
  isocodeFAO <- toolGetMapping("FAOiso_faocode.csv", where = "mrcommons")

  # Reads data
  data <- read.csv(file = paste(path.package("mrcommons"), paste0("extdata/sectoral/", file), sep = "/"))

  # Function to clean-up the data
  faoCleaning <- function(data = data, mapping = isocodeFAO,
                          subsetvar = "Unit..Name.", unitVar = "Tonnes - live weight", value = "Production") {

    yearsStats <- paste0("X.", 1984:2018, ".") # wide format
    # select needed columns
    if (value == "Production") {
      data <- data[, c("Country..Name.", "Unit..Name.", yearsStats)]
    } else if (value %in% c("exportsValue", "exportsQuantity")) {
      data <- data[, c("Country..Name.", "Trade.flow..Name.", "Unit..Name.", yearsStats)]
    }
    yearsStats <- as.character(1984:2018)
    if (value == "Production") {
      colnames(data) <- c("Country", "Variable", yearsStats)
    } else if (value %in% c("exportsValue", "exportsQuantity")) {
      colnames(data) <- c("Country", "Variable", "Unit", yearsStats)
    }
    data <- data[data$Variable == unitVar, ] # read only "Tonnes - live weight","Export"
    # from wide to long format
    data <- reshape(data, varying = yearsStats, direction = "long",
                    idvar = c("Country", "Variable"), v.names = "Value", timevar = "Year", times = yearsStats)
    rownames(data) <- seq_len(nrow(data)) # fix names of rows
    data <- merge(data, mapping, by = "Country")
    data <- data[, c("ISO3", "Year", "Value")]
    data[, "Year"] <- as.numeric(data[, "Year"])
    # converts to magpie object tonnes - live weight, current 1000 USD
    x <- magpiesort(as.magpie(data, temporal = 2, spatial = 1, datacol = 3))

    # remove historical countries; a more adequate solution is work in progress
    x <- x[c("ANT", "CSK", "SCG", "XET", "XSD"), , , invert = TRUE]

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
