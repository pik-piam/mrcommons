#' Read IMPACT3.2.2World_Price
#'
#' Read-in world prices data csv file as magclass object
#'
#'
#' @return magpie object of the world prices from the IMPACT model for different SSP scenarios
#' @author Mishko Stevanovic
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource(type = "IMPACT3.2.2World_Price")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom magclass as.magpie

readIMPACT3.2.2World_Price <- function() { # nolint: object_name_linter.
  data <- as.data.frame(read_excel("IMPACT 3.2.2 World Price-2.xlsx", skip = 6))
  data <- data[!is.na(data[[1]]), ]
  data$scenario <- sub(".", "p", data$scenario, fixed = TRUE)
  data <- as.magpie(data)

  #inflate to 2017 using US inflation for global value
  getItems(data, dim = 1) <- "USA"
  data <- GDPuc::toolConvertGDP(data,
                                unit_in = "constant 2005 US$MER",
                                unit_out = "constant 2017 US$MER",
                                replace_NAs = "no_conversion")
  getItems(data, dim = 1) <- "GLO"

  return(data)
}
