#' @title correctILOSTAT
#' @description changes currency unit where applicable
#' @param x unconverted magpie object from read-script
#' @param subtype Type of ILOSTAT data that should be read
#' @return Data as MAgPIE object with corrected currency units
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   a <- readSource("ILOSTAT", "EmplByActivityModelled", convert = "onlycorrect")
#' }
#' @importFrom magclass getNames<- getNames getSets
#' @importFrom GDPuc convertGDP


correctILOSTAT <- function(x, subtype) {
  # transform currencies if applicable
  if ("currency" %in% getSets(x)) {
    x[, , "2017 PPP $"] <- convertGDP(x[, , "2017 PPP $"],
                                      unit_in = "constant 2017 Int$PPP",
                                      unit_out = "constant 2005 Int$PPP")
    x[, , "US dollars"] <- convertGDP(x[, , "US dollars"],
                                      unit_in = "current US$MER",
                                      unit_out = "constant 2005 US$MER")
    getNames(x, dim = "currency")[getNames(x, dim = "currency") == "Local currency"] <- "current LCU"
    getNames(x, dim = "currency")[getNames(x, dim = "currency") == "2017 PPP $"] <- "US$PPP2005"
    getNames(x, dim = "currency")[getNames(x, dim = "currency") == "US dollars"] <- "US$MER2005"
  }

  # remove unknown country code
  if ("CHA" %in% getRegions(x)) {
    x <- x["CHA", , , invert = TRUE]
  }

  return(x)
}
