#' Read FRA2020
#'
#' Read-in an FRA (forest resource assessment) dataset from 2020.
#'
#' @param subtype data subtype. Available subtypes: forest_area, deforestation, growing_stock, biomass_stock,
#' carbon_stock, management, disturbance, forest_fire
#' @return Magpie object of the FRA 2020 data
#' @author Abhijeet Mishra
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("FRA2020", "growing_stock")
#' }
#'
#' @importFrom magclass as.magpie getItems
#' @importFrom madrat toolCountry2isocode
#' @export
readFRA2020 <- function(subtype) { # nolint
  # Capture source data
  fraData <- read.csv("FRA_Years_2020_12_01.csv", header = TRUE, dec = ".", na.strings = c("", " ", "NA", "."))
  colnames(fraData) <- gsub(pattern = "X", replacement = "", x = colnames(fraData), ignore.case = FALSE)
  allVariables <- colnames(fraData)
  id <- c("iso3", "year")

  subtypeList <- c("forest_area", "deforestation", "growing_stock", "biomass_stock", "carbon_stock", "management",
                   "disturbance", "forest_fire")
  if (!(subtype %in% subtypeList)) {
    stop("Invalid or unsupported subtype ", subtype, ". Accepted subtypes are ",
         paste(subtypeList, collapse = ", "), ". Choose one of the accepted subtype.")
  }

  # Some cleanup in read data may be necessary in case of missing or unreported data
  cleanData <- function(x) {
    # Read the csv source file, First row is info not needed by us
    data <- x

    # If data is not reported in any of the years, set it to 0,
    # if partial data is reported, set it to mean values of known data in that country
    varCount <- length(getNames(data))
    yrCount <- length(getYears(data))
    missingData <- 0
    partialData <- 0
    for (j in getItems(data, dim = 1.1)) {
      for (i in seq_len(varCount)) {
        naCount <- as.numeric(apply(data[j, , i], 1, function(x) sum(is.na(x))))
        if (naCount == yrCount) {
          missingData <- missingData + 1
          data[j, , i] <- 0
          missingData <- missingData
        } else if (naCount > 0 && naCount < yrCount) {
          partialData <- partialData + 1
          data[j, , i][is.na(data[j, , i])] <- mean(as.numeric(as.vector(data[j, , i])), na.rm = TRUE)
          partialData <- partialData
        }
      }
    }

    if (missingData > 0) {
      message(missingData, " missing data points.", " Such data will be set to 0.")
    }
    if (partialData > 0) {
      message(partialData, " partial data points.", " Such data will be set to mean value of reported data.")
    }

    # Replace X in colnames with y to make sure as.magpie recognizes this column as temporal dimension later
    colnames(data) <- gsub(pattern = "X", replacement = "y", x = colnames(data))

    # Return the cleaned data frame
    return(data)
  }

  switch(subtype,
         forest_area = {
           # Unit is 1000 ha
           identifiers <- c("1a_|1b_|1c_|1d_|1e_|1f_")
           variables <- grep(pattern = identifiers, x = allVariables, value = TRUE)
           data <- fraData[, allVariables %in% c(id, variables)]
           colnames(data) <- gsub(pattern = identifiers, replacement = "", x = colnames(data))
           out <- cleanData(as.magpie(data, spatial = "iso3"))
         },
         growing_stock = {
           # Unit is m3/ha or Mm3
           identifiers <- c("2a_")
           variables <- grep(pattern = identifiers, x = allVariables, value = TRUE)
           data <- fraData[, allVariables %in% c(id, variables)]
           colnames(data) <- gsub(pattern = identifiers, replacement = "", x = colnames(data))
           out <- cleanData(as.magpie(data, spatial = "iso3"))
         },
         biomass_stock = {
           # Unit is tDM/ha
           identifiers <- c("2c_")
           variables <- grep(pattern = identifiers, x = allVariables, value = TRUE)
           data <- fraData[, allVariables %in% c(id, variables)]
           colnames(data) <- gsub(pattern = identifiers, replacement = "", x = colnames(data))
           out <- cleanData(as.magpie(data, spatial = "iso3"))
         },
         carbon_stock = {
           # Unit is tC/ha
           identifiers <- c("2d_")
           variables <- grep(pattern = identifiers, x = allVariables, value = TRUE)

           # "2d_soil_depth_cm" not needed hence dropped
           data <- fraData[, allVariables %in% c(id, variables[-length(variables)])]

           colnames(data) <- gsub(pattern = identifiers, replacement = "", x = colnames(data))
           out <- cleanData(as.magpie(data, spatial = "iso3"))
         },
         management = {
           # Unit is 1000 ha
           identifiers <- c("3a_tot_")
           variables <- grep(pattern = identifiers, x = allVariables, value = TRUE)
           data <- fraData[, allVariables %in% c(id, variables)]
           colnames(data) <- gsub(pattern = identifiers, replacement = "", x = colnames(data))
           out <- cleanData(as.magpie(data, spatial = "iso3"))

           # Suriname has misreporting? The bad value is empty in 2020 from original file but
           # when it is read in R it takes bizarre values
           out["SUR", "y2020", ] <- out["SUR", "y2020", ] / 1000
         })

  # Some data does not belong to the bulk download and has to be downloaded manually
  # This data also needs some cleaning

  # Manual function to cleanup the data. Takes in source file as input
  processData <- function(x) {
    # Read the csv source file, First row is info not needed by us
    data <- read.csv(x, header = TRUE, skip = 1)

    # Manually rename first column
    colnames(data)[1] <- "Country"

    # Cleanup additional name info
    data$Country <- # nolint
      gsub(pattern = " \\(French Part\\)| \\(Desk study\\)", replacement = "", x = data$Country)

    # Convert from country names to ISO codes
    data$Country <- # nolint
      suppressWarnings(toolCountry2isocode(data$Country,
                                           warn = TRUE,
                                           mapping = c("Saint-Martin" = "MAF"),
                                           ignoreCountries = c("2020-12-22", "2021-09-10",
                                                               "\u00A9 FRA 2020", "\u00A9 FRA 2021")
      ))

    # Cleanup rows with NA in country names - Rows with no matching ISO code will be dropped
    data <- data[!is.na(data$Country), ]

    # If data is not reported in any of the years, set it to 0,
    # if partial data is reported, set it to mean values of known data in that country
    yrCount <- ncol(data)
    missingData <- 0
    partialData <- 0
    for (i in seq_len(nrow(data))) {
      naCount <- as.numeric(apply(data[i, ], 1, function(x) sum(is.na(x))))
      if (naCount == yrCount - 1) {
        missingData <- missingData + 1
        data[i, -1] <- 0
        missingData <- missingData
      } else if (naCount > 0 && naCount < yrCount - 1) {
        partialData <- partialData + 1
        data[i, is.na(data[i, ])] <- mean(as.numeric(as.vector(data[i, -1])), na.rm = TRUE)
        partialData <- partialData
      }
    }
    if (missingData > 0) {
      cat(missingData, " missing data points.", " Such data will be set to 0.")
    }
    if (partialData > 0) {
      cat(partialData, " partial data points.", " Such data will be set to mean value of reported data.")
    }

    # Replace X in colnames with y to make sure as.magpie recognizes this column as temporal dimension later
    colnames(data) <- gsub(pattern = "X", replacement = "y", x = colnames(data))

    # Return the cleaned data frame
    return(data)
  }

  switch(subtype,
         disturbance = {
           # Unit is 1000 ha
           out <- as.magpie(processData("fra2020-disturbances.csv"))
         },
         forest_fire = {
           # Unit is 1000 ha
           out <- as.magpie(processData("fra2020-areaAffectedByFire.csv"))
         },
         deforestation = {
           # Capture source data for deforestation -- Not available in the bulk download
           # Unit is 1000 ha/yr
           tempDat <- processData("fra2020-forestAreaChange.csv")
           colnames(tempDat) <- c("Country", "y1990", "y2000", "y2010", "2015")
           out <- as.magpie(tempDat)
           out2 <- out[, "y2010", , invert = TRUE]
           getYears(out2) <- c("y1995", "y2005", "y2020")
           out <- mbind(out, out2)
           out <- out[, sort(getYears(out)), ]
         })
  out <- out[grep(pattern = "X0|X1|X2", x = getItems(out, dim = 1.1), value = TRUE, invert = TRUE), , ]
  return(out)
}
