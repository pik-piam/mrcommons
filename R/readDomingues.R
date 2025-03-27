#' @title readDomingues
#' @description Reads biomass yields (GJ/ha/year) and factor costs (US$ 2018/GJ)
#' for bioenergy crops from Domingues et al., 2022 (DOI: 10.5281/zenodo.6913709)
#' @return Data as MAgPIE object with average yields and costs of betr and begr
#' @author Eva Bleidorn
#' @importFrom readxl read_excel
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr recode
#' @importFrom dplyr filter

readDomingues <- function() {
  # File
  file <- "DATABASE_BIOMASS_COST.xlsx"

  # Columns to extract and their location in the file
  names <- c("ID", "biomassType", "species", "Country", "yield (GJ/ha/year)",
             "capitalCost (US$2018/GJ)", "labourCost (US$2018/GJ)", "fuelCost (US$2018/GJ)",
             "fertilizersPesticidesCost (US$2018/GJ)", "unspecifiedCostWithTransportation (US$2018/GJ)",
             "unspecifiedCostWithoutTransportation (US$2018/GJ)",
             "totalCostWithTransportation (US$2018/GJ)",
             "totalCostWithoutTransportation (US$2018/GJ)")

  ranges <- c("A3:A430", "B3:B430", "C3:C430", "I3:I430", "W3:W430", "AZ3:AZ430", "BA3:BA430", "BB3:BB430",
              "BC3:BC430", "BD3:BD430", "BE3:BE430", "BF3:BF430", "BG3:BG430")




  # Function to extract the data from the Excel sheet
  extractData <- function(ranges, file) {
    dataList <- list()

    for (n in seq_along(ranges)) {
      # Read data from each range in the Excel file
      dataInt <- as.data.frame(read_excel(file, sheet = "Data 2018 USD", range = ranges[n]))

      # Assign new column name to the data
      colnames(dataInt) <- names[n]

      # Append the data frame to the list
      dataList[[n]] <- dataInt
    }

    # Combine all columns from the list into one data frame
    dataCombined <- do.call(cbind, dataList)
    return(dataCombined)
  }

  # Extract the data from the excel sheet
  data <- extractData(ranges, file)

  # Read country mapping: ISO3 to countries
  mapping <- toolGetMapping("countryMappingDomingues.csv", where = "mrcommons", type = "regional")

  # Merge isocodes to the data by "Country" (coutnry names in long format)
  mergedData <- left_join(data, mapping, by = "Country") %>%

    # Replace all 0 values with NA
    mutate(across(dplyr::everything(), ~ replace(., . == 0, NA))) %>%

    # recode WEC to betr and GEC to begr
    mutate(biomassType = dplyr::recode(.data$biomassType,
                                       "WEC" = "betr",
                                       "GEC" = "begr")) %>%

    # remove species = "Invader bushes" (because they are not purpose-grown biomass)
    dplyr::filter(.data$species != "Invader bushes")



  # Create magpie object
  t <- as.magpie(mergedData[!is.na(mergedData$ISO3), setdiff(colnames(mergedData), c("Country"))], spatial = "ISO3")

  # keep only biomassType betr and begr (wood and grass energy crops)
  t <- t[, , c("betr", "begr"), drop = TRUE]



  # calculate sum per country-variable-combination (sum over species and IDs)
  t2 <- dimSums(t, dim = c(3.1, 3.3), na.rm = TRUE)

  # calculate n (number of non-NA values) for each country-variable-combination
  num <- dimSums(!is.na(t), dim = c(3.1, 3.3), na.rm = TRUE)

  # calculate mean and replace NaN with NA (all country-variable combinations where there were no values in the dataset)
  x <- t2 / num
  x[is.nan(x)] <- NA


  return(x)
}
