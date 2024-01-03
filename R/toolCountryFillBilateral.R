#' @title toolCountryFillBilateral
#' @description Fills bilateral iso-level magpie objects to 249 x 249 countries
#' @param x input variable, a bilateral magclass object
#' @param fill fill value, default NA
#' @export

toolCountryFillBilateral <- function(x, fill = NA) {
  isoCountry <- read.csv2(system.file("extdata", "iso_country.csv", package = "madrat"), row.names = NULL)
  countrylist <- as.vector(isoCountry[, "x"])
  names(countrylist) <- isoCountry[, "X"]
  full <- expand.grid(countrylist, countrylist)
  full <- paste0(as.character(full[[1]]), ".", as.character(full[[2]]))
  missing <- setdiff(full, getItems(x, dim = 1))
  if (length(missing) > 0) {
    x <- mbind(x, new.magpie(cells_and_regions = missing,
                             years = getYears(x), names = getNames(x), fill = fill))
  }
  return(x)
}
