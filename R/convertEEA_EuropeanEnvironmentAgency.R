#' Convert European Environment Agency (EEA) data
#'
#' Read-in European Environment Agency (EEA) data on ETS emissions as magclass object
#'
#' @param x MAgPIE object to be converted
#' @param subtype data subtype. Either "ETS", "historical", "projections", or "projections-detailed"
#' @return magpie object of European Environment Agency (EEA) ETS emissions (GtCO2)
#' @author Renato Rodrigues, Robin Hasse
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "EEA_EuropeanEnvironmentAgency", subtype = "ETS")
#' }
#'
#' @importFrom magclass as.magpie getItems getItems<-
#' @importFrom madrat toolCountry2isocode toolCountryFill
#'

convertEEA_EuropeanEnvironmentAgency <- function(x, subtype) { # nolint: object_name_linter.
  if (subtype %in% c("ETS", "ESR")) {
    # fill up zero countries
    x <- toolCountryFill(x, verbosity = 2)
    # remove NAs
    x[is.na(x)] <- 0
  } else if (subtype == "total") {
    x <- toolCountryFill(x, verbosity = 2)
  } else if (subtype == "sectoral") {
    x <- toolCountryFill(x, verbosity = 2, no_remove_warning = "EUR")
  } else if (subtype %in% c("projections", "projections-detailed", "ghgEmissionIntensityElec")) {
    getItems(x, 1) <- toolCountry2isocode(getItems(x, 1), warn = FALSE, mapping = c("EL" = "GRC"))
    x <- toolCountryFill(x, verbosity = 2, no_remove_warning = NA)
  }
  return(x)
}
