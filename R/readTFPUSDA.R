#' @title readTFPUSDA
#' @description Reads the input shares from USDA's Agricultural total factor productivity growth indices assessment.
#'
#'
#'
#'
#'
#' @return magpie object with fractions of different input factors in the overall production value
#' @author Edna J. Molina Bacca
#' @importFrom readxl read_excel
#' @importFrom stats reshape
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("TFP_USD")
#' }
#'
readTFPUSDA <- function() {

 # File
  file <- "AgTFPindividualcountries.xlsx"

  # Reads countries from the file
  countries <- read_excel(file, sheet = "Cost Shares", range = "D3:D182")
  FAON_countries <- read_excel(file, sheet = "Cost Shares", range = "B3:B182")
  regions <- cbind(countries, FAON_countries)
  colnames(regions) <- c("Country/territory", "FAO N")

  # Available shares and their location in the file
  names <- c("AG_Labour", "AG_Land", "Livestock", "Machinery", "Materials_Crops", "Materials_Animals")
  ranges <- c("S3:X182", "AA3:AF182", "BG3:BL182", "AY3:BD182", "BO3:BT182", "BW3:CB182")

  # Function to extract the values of the shares and organize the data in a format easy to convert to a magpie object
  extract_fractions <- function(names = names, ranges = ranges, file = file, regions = regions) {
    data <- NULL
    years_s <- as.character(seq(from = 1960, to = 2010, by = 10))

    for (n in 1:length(names)) {
      data_int <- as.data.frame(read_excel(file, sheet = "Cost Shares", range = ranges[n]))
      colnames(data_int) <- years_s
      data_int <- cbind(regions, data_int)
      data_int$Input <- names[n]
      data <- rbind(data, data_int)
    }
    data <- reshape(data, varying = years_s, direction = "long", idvar = c("Country/territory", "FAO N", "Input"), v.names = "Value", timevar = "Year", times = years_s)
    rownames(data) <- 1:length(rownames(data))
    colnames(data) <- c("Country", "CountryCode", "Input", "Year", "Value")
    return(data)
  }

  # Extracts data
  data <- extract_fractions(names, ranges, file, regions)

  # Reads fao countries
  isocode_FAO <- toolGetMapping("FAOiso_faocode.csv", where = "mrcommons")

  # Merges read data with the fao mapping by country code
  data <- merge(data, isocode_FAO, by = "CountryCode", all = FALSE)[, c("ISO3", "Year", "Input", "Value")]

  # Creates magpie object (184 countries)
  x <- magpiesort(as.magpie(data, spatial = 1, temporal = 2, datacol = 4))

  # Filling HKG and SGP with developed Asia values
  FillDevAsia <- new.magpie(cells_and_regions = c("HKG", "SGP"), years = getYears(x), names = getNames(x))
  FillDevAsia[, , ] <- x["TWN", , ]
  x <- mbind(x, FillDevAsia)

  # Fills with zeros the countries that were not reported
  x <- toolCountryFill(x, fill = 0)




  return(x)
}
