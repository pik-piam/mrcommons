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
#' @importFrom data.table fread :=
#' @importFrom dplyr %>%
#' @importFrom madrat toolCountry2isocode
#'
readIEA <- function(subtype) {
  if (subtype == "EnergyBalances") { # IEA energy balances until 2020 (incomplete 2021) (data updated in August, 2022)

    energyBalancesFile <- "IEA-Energy-Balances-2022/worldbig.csv"
    data <- fread(
      file = energyBalancesFile,
      col.names = c("COUNTRY", "PRODUCT", "FLOW", "TIME", "ktoe"),
      colClasses = c("character", "character", "character", "numeric", "character"),
      sep = ";", stringsAsFactors = FALSE, na.strings = c("x", "..", "c"), skip = 2, showProgress = FALSE
    )

    customMapping <- c(
      "AUSTRALI" = "AUS",
      "BOSNIAHERZ" = "BIH",
      "CONGOREP" = "COD",
      "COTEIVOIRE" = "CIV",
      "ELSALVADOR" = "SLV",
      "EQGUINEA" = "GNQ",
      "DOMINICANR" = "DOM",
      "FSUND" = "SUN",
      "HONGKONG" = "HKG",
      "KOREADPR" = "PRK",
      "LUXEMBOU" = "LUX",
      "NETHLAND" = "NLD",
      "NORTHMACED" = "MKD",
      "PHILIPPINE" = "PHL",
      "SAUDIARABI" = "SAU",
      "SOUTHAFRIC" = "ZAF",
      "SSUDAN" = "SSD",
      "SRILANKA" = "LKA",
      "SWITLAND" = "CHE",
      "TAIPEI" = "TWN",
      "TURKMENIST" = "TKM",
      "TRINIDAD" = "TTO",
      "UAE" = "ARE",
      "YUGOND" = "YUG",
      "OTHERAFRIC" = "IAF",
      "OTHERASIA" = "IAS",
      "OTHERLATIN" = "ILA"
    )

    data$COUNTRY <- toolCountry2isocode(data$COUNTRY, mapping = customMapping, warn = FALSE)
    data <- data %>%
      filter(!is.na(!!sym("ktoe")), !is.na(!!sym("COUNTRY"))) %>%
      mutate(!!sym("ktoe") := as.numeric(!!sym("ktoe")))

    mdata <- as.magpie(data,
                       datacol = dim(data)[2], spatial = which(colnames(data) == "COUNTRY"),
                       temporal = which(colnames(data) == "TIME")
    )

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
