#' @title convertILOSTAT
#' @description transforms currencies where applicable, and fills missing countries in ILOSTAT data with 0
#' @param x unconverted magpie object from read-script
#' @return Data as MAgPIE object with common country list
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   a <- readSource("ILOSTAT", subtype = "EmplByActivityModelled", convert = TRUE)
#' }

convertILOSTAT <- function(x) {

  # fill missing countries
  x <- toolCountryFill(x, fill = 0)

  # transform currencies if applicable
  if ("currency" %in% getSets(x)) {
    xUnconverted <- x
    x[, , "2017 PPP $"] <- convertGDP(x[, , "2017 PPP $"],
                                      unit_in = "constant 2017 Int$PPP",
                                      unit_out = "constant 2005 Int$PPP")
    x[, , "US dollars"] <- convertGDP(x[, , "US dollars"],
                                      unit_in = "current US$MER",
                                      unit_out = "constant 2005 US$MER")

    # for countries with missing conversion factors we assume no inflation
    x[is.na(x)] <- xUnconverted[is.na(x)]

    # update unit description
    getNames(x, dim = "currency")[getNames(x, dim = "currency") == "Local currency"] <- "current LCU"
    getNames(x, dim = "currency")[getNames(x, dim = "currency") == "2017 PPP $"] <- "US$PPP2005"
    getNames(x, dim = "currency")[getNames(x, dim = "currency") == "US dollars"] <- "US$MER2005"
  }

  # set missing values to 0
  x[!is.finite(x)] <- 0

  return(x)
}
