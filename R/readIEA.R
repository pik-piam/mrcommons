#' Read IEA
#'
#' Read-in an IEA csv file as magpie object
#'
#' @param subtype data subtype. Either "EnergyBalances", "EnergyBalances-2023", or "Emissions".
#' - "EnergyBalances": IEA energy balances until 2020 (incomplete 2021), data updated in Aug 2022,
#' the current default for REMIND input data
#' - "EnergyBalances-latest": IEA energy balances until 2021 (incomplete 2022), data updated in Aug 2023,
#' currently used for comparisons only
#' @return magpie object of the IEA
#' @author Anastasis Giannousakis, Lavinia Baumstark, Renato Rodrigues, Falk Benke
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

  if (grepl("EnergyBalances", subtype)) {

    if (subtype == "EnergyBalances") {
      # the current default :
      energyBalancesFile <- "IEA-Energy-Balances-2022/worldbig.csv"
      incomplete <- 2021
    } else if (subtype == "EnergyBalances-latest") {
      energyBalancesFile <- "IEA-Energy-Balances-2023/worldbig.csv"
      incomplete <- 2022
    } else {
      stop("Invalid subtype!")
    }

    data <- fread(
      file = energyBalancesFile,
      col.names = c("COUNTRY", "PRODUCT", "FLOW", "TIME", "ktoe"),
      colClasses = c("character", "character", "character", "numeric", "character"),
      sep = ";", stringsAsFactors = FALSE, na.strings = c("x", "..", "c"), skip = 2, showProgress = FALSE
    )

    data$COUNTRY <- toolCountry2isocode(data$COUNTRY, warn = FALSE)

    data <- data %>%
      filter(!is.na(!!sym("ktoe")),
             !is.na(!!sym("COUNTRY")),
             !!sym("TIME") !=  incomplete) %>% # exclude latest year with incomplete data
      mutate(!!sym("ktoe") := as.numeric(!!sym("ktoe")))

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

  } else {
    stop("Invalid subtype!")
  }
  return(mdata)
}
