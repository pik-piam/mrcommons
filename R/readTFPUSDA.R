#' @title readTFPUSDA
#' @description Reads the input shares from USDA's Agricultural total factor productivity growth indices assessment.
#'
#' @return magpie object with fractions of different input factors in the overall production value
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' a <- readSource("TFP_USD")
#' }
#' @seealso [readSource()]
#' @importFrom readxl read_excel
#' @importFrom stats reshape
readTFPUSDA <- function() { # nolint
  dataFile <- "AgTFPindividualcountries.xlsx"

  countries <- read_excel(dataFile, sheet = "Factor Shares", range = "D3:D190", progress = FALSE)
  countriesFAON <- read_excel(dataFile, sheet = "Factor Shares", range = "B3:B190", progress = FALSE)
  regions <- cbind(countries, countriesFAON)

  # Available shares and their location in the dataFile
  names <- c("revenue", "AG_Labour", "AG_Land", "Livestock", "Machinery", "Materials_Crops", "Materials_Animals")
  ranges <- c("K3:P190", "R3:W190", "Y3:AD190", "AF3:AK190", "AM3:AR190", "AT3:AY190", "BA3:BF190")

  # Function to extract the values of the shares and organize the data in a format easy to convert to a magpie object
  extractFractions <- function(names = names, ranges = ranges, file = dataFile, regions = regions) {
    x <- NULL
    yearsS <- as.character(seq(from = 1960, to = 2010, by = 10))

    for (n in seq_along(names)) {
      dataInt <- read_excel(dataFile, sheet = "Factor Shares", range = ranges[n], progress = FALSE)
      colnames(dataInt) <- yearsS
      dataInt <- cbind(regions, dataInt)
      dataInt[["Input"]] <- names[n]
      x <- rbind(x, dataInt)
    }
    x <- reshape(x, varying = yearsS, direction = "long", idvar = c("Country/territory", "FAO N", "Input"),
                 v.names = "Value", timevar = "Year", times = yearsS)
    rownames(x) <- seq_along(rownames(x))
    colnames(x) <- c("Country", "CountryCode", "Input", "Year", "Value")
    return(x)
  }

  # Extracts data
  x <- extractFractions(names, ranges, dataFile, regions)

  # Reads fao countries
  isocodeFAO <- toolGetMapping("FAOiso_faocode.csv", where = "mrcommons")

  # Merges read data with the fao mapping by country code
  x <- merge(x, isocodeFAO, by = "CountryCode", all = FALSE)[, c("ISO3", "Year", "Input", "Value")]

  x[x$ISO3 == "XCN", "ISO3"] <- "CHN"

  # Creates magpie object (184 countries)
  x <- magpiesort(as.magpie(x, spatial = 1, temporal = 2, datacol = 4))

  # assume SSD/South Sudan and SDN/Sudan have same shares as XSD/Former Sudan
  x <- add_columns(x, c("SSD", "SDN"), dim = 1.1)
  x["SSD", , ] <- x["XSD", , ]
  x["SDN", , ] <- x["XSD", , ]

  # remove historical countries, data for corresponding new countries already exists
  x <- x[c("SUN", "YUG", "XSD", "CSK", "SCG", "XBL", "XET"), , , invert = TRUE]

  # Fills with zeros the countries that were not reported, remove historical countries
  x <- toolCountryFill(x, fill = 0)
  return(x)
}
