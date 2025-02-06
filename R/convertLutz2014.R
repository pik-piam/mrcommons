#' @title convertLutz2014
#'
#' @description It fills the missing values of the output of readLutz2014 through
#' the weighted average of the values of two countries with similar characteristics
#' to the one that has na values.
#' @param x magpie object provided by the read function
#'
#' @seealso
#' [readLutz2014()]


convertLutz2014 <- function(x) {
  # handle countries with missing education data before 2010
  # missing years
  missing <- paste0("y", 1965 + (1:9) * 5)
  # keep education structure constant over time
  x[, missing, ] <- setYears(x[, "y2010", ] / x[, "y2010", "Total"], NULL) * x[, missing, "Total"]
  x[is.nan(x)] <- 0

  x <- toolCountryFill(x, fill = NA, no_remove_warning = "ANT")

  # BB: use of toolAggregate with an external mapping could replace the following function and speed it up
  popWdi <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)
  fillCountryByAverageOfRegion <- function(x, country, region) {
    vcat(2, paste0("interpolating country: ", country))
    values <- x[region, , ]
    population <- popWdi[country, getYears(values), ]
    average <- dimSums(values, dim = 1) / dimSums(values[, , "Total"][, , "Both"][, , "All"], dim = 1)
    x[country, , ]  <-  setCells(average, "GLO") * population
    return(x)
  }

  x <- fillCountryByAverageOfRegion(x, country = "AIA", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "ALA", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "ATG", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "AND", region = c("FRA", "ESP"))
  x <- fillCountryByAverageOfRegion(x, country = "ASM", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "ATA", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "ATF", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "BES", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "BLM", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "BMU", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "BVT", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "CCK", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "COK", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "CUW", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "CXR", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "CYM", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "DMA", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "ESH", region = c("MRT", "MLI"))
  x <- fillCountryByAverageOfRegion(x, country = "FLK", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "FRO", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "GGY", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "GIB", region = c("GBR", "ESP"))
  x <- fillCountryByAverageOfRegion(x, country = "GRL", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "HMD", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "IMN", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "IOT", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "JEY", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "KIR", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "KNA", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "LIE", region = c("CHE", "LUX"))
  x <- fillCountryByAverageOfRegion(x, country = "MAF", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "MCO", region = c("CHE", "LUX"))
  x <- fillCountryByAverageOfRegion(x, country = "MHL", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "MNP", region = c("NCL", "WSM")) #
  x <- fillCountryByAverageOfRegion(x, country = "MSR", region = c("PRI")) #
  x <- fillCountryByAverageOfRegion(x, country = "NFK", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "NIU", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "NRU", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "PCN", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "PLW", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "SGS", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "SHN", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "SJM", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "SMR", region = c("CHE", "LUX"))
  x <- fillCountryByAverageOfRegion(x, country = "SPM", region = c("ISL", "EST"))
  x <- fillCountryByAverageOfRegion(x, country = "SSD", region = c("TCD", "SDN"))
  x <- fillCountryByAverageOfRegion(x, country = "SXM", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "SYC", region = c("MUS", "MDV"))
  x <- fillCountryByAverageOfRegion(x, country = "TCA", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "TKL", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "TUV", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "TWN", region = c("KOR", "HKG"))
  x <- fillCountryByAverageOfRegion(x, country = "UMI", region = c("NCL", "WSM"))
  x <- fillCountryByAverageOfRegion(x, country = "VAT", region = c("CHE", "LUX"))
  x <- fillCountryByAverageOfRegion(x, country = "VGB", region = c("PRI"))
  x <- fillCountryByAverageOfRegion(x, country = "WLF", region = c("NCL", "WSM"))

  # change unit to million

  # Replacing NAs by zeros leads to problems later-on in the code.
  # Please do a proper replacement as for the other countries!
  return(x)
}
