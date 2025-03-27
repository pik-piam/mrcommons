#' @title convertDomingues
#' @description fills missing countries in Domingues data with NA, and transforms
#' cost units from 2018 US$/GJ to 2017 US$/GJ
#' @param x unconverted magpie object from read-script
#' @return Data as MAgPIE object with common country list
#' @author Eva Bleidorn


convertDomingues <- function(x) {
  # fill missing countries
  x <- toolCountryFill(x, fill = NA)


  # transform all cost variables from US$2018/GJ to US$2017/GJ
  variables <- getNames(x, dim = "variable")
  costVariables <- getNames(x, dim = "variable")[grep("\\(US\\$2018/GJ\\)$", variables)]

  x[, , costVariables] <- toolConvertGDP(x[, , costVariables],
                                         unit_in = "constant 2018 US$MER",
                                         unit_out = "constant 2017 US$MER",
                                         replace_NAs = c("linear", "no_conversion"))

  # update variable names (replace "2018" with "2017")
  getNames(x, dim = "variable")[grepl("US\\$2018/GJ", getNames(x, dim = "variable"))] <-
    gsub("2018", "2017", getNames(x, dim = "variable")[grepl("US\\$2018/GJ", getNames(x, dim = "variable"))])


  return(x)
}
