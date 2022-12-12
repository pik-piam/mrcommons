#' Read IEA
#'
#' Read-in an IEA csv file as magpie object
#'
#' @param subtype data subtype. Either "EnergyBalances", "CHPreport" or "Emissions")
#' @return magpie object of the IEA
#' @author Anastasis Giannousakis, Lavinia Baumstark, Renato Rodrigues
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource(type = "IEA", subtype = "EnergyBalances")
#' }
#'
#' @importFrom data.table fread
#' @importFrom madrat toolCountry2isocode
#' @importFrom R.utils decompressFile
#'
readIEA <- function(subtype) {
  if (subtype == "EnergyBalances") { # IEA energy balances until 2015 (estimated 2016) (data updated in February, 2018)
    energyBalancesFile <- tempfile(fileext = "csv")
    decompressFile("IEA-Energy-Balances-2016/ExtendedEnergyBalances.csv.gz",
                   energyBalancesFile, remove = FALSE, ext = "not_used",
                   FUN = gzfile)
    data <- fread(file = energyBalancesFile,
                  col.names = c("COUNTRY", "PRODUCT", "FLOW", "TIME", "ktoe"),
                  colClasses = c("character", "character", "character", "numeric", "character"),
                  sep = ";", stringsAsFactors = FALSE, na.strings = c("x", ".."), skip = 2, showProgress = FALSE)
    unlink(energyBalancesFile)
    # converting IEA country names to ISO codes
    data$COUNTRY <- toolCountry2isocode(data$COUNTRY, warn = FALSE) # nolint
    # removing NAs and converting data to numeric type
    data <- data[!is.na(data$ktoe), ]
    data$ktoe <- suppressWarnings(as.numeric(data$ktoe))
    # creating MAgPIE IEA energy balances object
    mdata <- as.magpie(data, datacol = dim(data)[2], spatial = which(colnames(data) == "COUNTRY"),
                       temporal = which(colnames(data) == "TIME"))
  } else if (subtype == "Emissions") {
    data <- read.csv("emissions2013.csv")
    data$COUNTRY <- toolCountry2isocode(data$COUNTRY, warn = FALSE) # nolint
    data <- data[!is.na(data$COUNTRY), ]
    data$TIME <- paste("y", data$TIME, sep = "") # nolint
    if (names(data)[[5]] == "MtCO2") data <- data[!is.na(data$MtCO2), ]
    if (names(data)[[5]] == "MtCO2") data$MtCO2 <- suppressWarnings(as.numeric(data$MtCO2)) # nolint
    mdata <- as.magpie(data, datacol = dim(data)[2])
  } else if (subtype == "CHPreport") {
    data <- read.csv("CHPdata.csv", sep = ";", skip = 4)
    data$CountryName <- NULL # nolint
    mdata <- as.magpie(data, datacol = 2)
    getNames(mdata) <- "chp share"
  } else {
    stop("Not a valid subtype!")
  }
  return(mdata)
}
