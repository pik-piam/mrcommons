#' Read FRA2020
#'
#' Read-in an FRA data from 2020 (forest resource assessment).
#'
#'
#' @param subtype data subtype. Available subtypes: "forest_area","growing_stock","biomass_stock","carbon_stock","management","disturbance","forest_fire"
#' @return Magpie object of the FRA 2020 data
#' @author Abhijeet Mishra
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("FRA2020", "growing_stock")
#' }
#'
#' @importFrom magclass as.magpie getRegions
#' @importFrom madrat toolSubtypeSelect toolCountry2isocode
#' @import countrycode
#' @importFrom stats complete.cases
#' @export

readFRA2020 <- function(subtype) {

  ## Capture source data
  file <- "FRA_Years_2020_12_01.csv"
  fra_data <- read.csv(file, header = TRUE, dec = ".", na.strings = c("", " ", "NA", "."))
  colnames(fra_data) <- gsub(pattern = "X", replacement = "", x = colnames(fra_data), ignore.case = FALSE)
  all_variables <- colnames(fra_data)
  id <- c("iso3", "year")

  subtype_list <- c("forest_area", "deforestation", "growing_stock", "biomass_stock", "carbon_stock", "management", "disturbance", "forest_fire")

  ### Some cleanup in read data may be necessary in case of missing or unreported data

  clean_data <- function(x) {

    # Read the csv source file, First row is info not needed by us
    data <- x

    # If data is not reported in any of the years, set it to 0,
    # if partial data is reported, set it to mean values of known data in that country
    var_count <- length(getNames(data))
    yr_count <- length(getYears(data))
    missing_data <- 0
    partial_data <- 0
    for (j in getRegions(data)) {
      for (i in 1:var_count) {
        na_count <- as.numeric(apply(data[j, , i], 1, function(x) sum(is.na(x))))
        if (na_count == yr_count) {
          missing_data <- missing_data + 1
          data[j, , i] <- 0
          missing_data <- missing_data
        } else if (na_count > 0 & na_count < yr_count) {
          partial_data <- partial_data + 1
          data[j, , i][is.na(data[j, , i])] <- mean(as.numeric(as.vector(data[j, , i])), na.rm = TRUE)
          partial_data <- partial_data
        }
      }
    }

    if (missing_data > 0) message(missing_data, " missing data points.", " Such data will be set to 0.")
    if (partial_data > 0) message(partial_data, " partial data points.", " Such data will be set to mean value of reported data.")

    # Replace X in colnames with y to make sure as.magpie recognizes this column as temporal dimension later
    colnames(data) <- gsub(pattern = "X", replacement = "y", x = colnames(data))

    # Return the cleaned data frame
    return(data)
  }

  if (subtype == "forest_area") {
    ## Unit = 1000 ha
    identifiers <- c("1a_|1b_|1c_|1d_|1e_|1f_")
    variables <- grep(pattern = identifiers, x = all_variables, value = TRUE)
    data <- fra_data[, all_variables %in% c(id, variables)]
    colnames(data) <- gsub(pattern = identifiers, replacement = "", x = colnames(data))
    out <- clean_data(as.magpie(data, spatial = "iso3"))
  }

  if (subtype == "growing_stock") {
    ## Unit = m3/ha or Mm3
    identifiers <- c("2a_")
    variables <- grep(pattern = identifiers, x = all_variables, value = TRUE)
    data <- fra_data[, all_variables %in% c(id, variables)]
    colnames(data) <- gsub(pattern = identifiers, replacement = "", x = colnames(data))
    out <- clean_data(as.magpie(data, spatial = "iso3"))
  }

  if (subtype == "biomass_stock") {
    ## Unit = tDM/ha
    identifiers <- c("2c_")
    variables <- grep(pattern = identifiers, x = all_variables, value = TRUE)
    data <- fra_data[, all_variables %in% c(id, variables)]
    colnames(data) <- gsub(pattern = identifiers, replacement = "", x = colnames(data))
    out <- clean_data(as.magpie(data, spatial = "iso3"))
  }

  if (subtype == "carbon_stock") {
    ## Unit = tC/ha
    identifiers <- c("2d_")
    variables <- grep(pattern = identifiers, x = all_variables, value = TRUE)
    data <- fra_data[, all_variables %in% c(id, variables[-length(variables)])] ## "2d_soil_depth_cm" not needed hence dropped
    colnames(data) <- gsub(pattern = identifiers, replacement = "", x = colnames(data))
    out <- clean_data(as.magpie(data, spatial = "iso3"))
  }

  if (subtype == "management") {
    ## Unit = 1000 ha
    identifiers <- c("3a_tot_")
    variables <- grep(pattern = identifiers, x = all_variables, value = TRUE)
    data <- fra_data[, all_variables %in% c(id, variables)]
    colnames(data) <- gsub(pattern = identifiers, replacement = "", x = colnames(data))
    out <- clean_data(as.magpie(data, spatial = "iso3"))
    out["SUR", "y2020", ] <- out["SUR", "y2020", ] / 1000 ## Suriname has misreporting? The bad value is empty in 2020 from original file but when it is read in R it takes bizarre values
  }


  ##### Some data does not belong to the bulk download and has to be downloaded manually
  ##### This data also needs some cleaning

  ## Manual function to cleanup the data. Takes in source file as input
  process_data <- function(x) {

    # Read the csv source file, First row is info not needed by us
    data <- read.csv(x, header = TRUE, skip = 1)

    # Manually rename first column
    colnames(data)[1] <- "Country"

    # Cleanup additional name info
    data$Country <- gsub(pattern = " \\(French Part\\)| \\(Desk study\\)", replacement = "", x = data$Country)

    # Convert from country names to ISO codes
    data$Country <- suppressWarnings(toolCountry2isocode(data$Country,
      warn = TRUE,
      mapping = c("Saint-Martin" = "MAF"),
      ignoreCountries = c("2020-12-22", "\u00C2\u00A9 FRA 2020")
    ))

    # Cleanup rows with NA in country names - Rows with no matching ISO code will be dropped
    data <- data[!is.na(data$Country), ]

    # If data is not reported in any of the years, set it to 0,
    # if partial data is reported, set it to mean values of known data in that country
    yr_count <- ncol(data)
    missing_data <- 0
    partial_data <- 0
    for (i in 1:nrow(data)) {
      na_count <- as.numeric(apply(data[i, ], 1, function(x) sum(is.na(x))))
      if (na_count == yr_count - 1) {
        missing_data <- missing_data + 1
        data[i, -1] <- 0
        missing_data <- missing_data
      } else if (na_count > 0 & na_count < yr_count - 1) {
        partial_data <- partial_data + 1
        data[i, is.na(data[i, ])] <- mean(as.numeric(as.vector(data[i, -1])), na.rm = TRUE)
        partial_data <- partial_data
      }
    }
    if (missing_data > 0) cat(missing_data, " missing data points.", " Such data will be set to 0.")
    if (partial_data > 0) cat(partial_data, " partial data points.", " Such data will be set to mean value of reported data.")

    # Replace X in colnames with y to make sure as.magpie recognizes this column as temporal dimension later
    colnames(data) <- gsub(pattern = "X", replacement = "y", x = colnames(data))

    # Return the cleaned data frame
    return(data)
  }

  if (subtype == "disturbance") {
    ## Unit 1000ha
    file <- "fra2020-disturbances.csv"
    out <- as.magpie(process_data(file))
  }

  if (subtype == "forest_fire") {
    ## Unit 1000 ha
    file <- "fra2020-areaAffectedByFire.csv"
    out <- as.magpie(process_data(file))
  }

  if (subtype == "deforestation") {
    ## Capture source data for deforestation -- Not available in the bulk download
    ## Unit = 1000 ha/yr
    file <- "fra2020-forestAreaChange.csv"
    temp_dat <- process_data(file)
    colnames(temp_dat) <- c("Country", "y1990", "y2000", "y2010", "2015")
    out <- as.magpie(temp_dat)
    out2 <- out[, "y2010", , invert = TRUE]
    getYears(out2) <- c("y1995", "y2005", "y2020")
    out <- mbind(out, out2)
    out <- out[, sort(getYears(out)), ]
  } else if (!(subtype %in% subtype_list)) {
    stop("Invalid or unsupported subtype ", subtype, ". Accepted subtypes are ", paste(subtype_list, collapse = ", "), ". Choose one of the accepted subtype. Returning NULL")
  }
  out <- out[grep(pattern = "X0|X1|X2", x = getRegions(out), value = TRUE, invert = TRUE), , ]
  return(out)
}
