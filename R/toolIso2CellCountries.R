#' toolIso2CellCountries
#'
#' Select country names of countries which are present on cellular level
#' @param x magpie object on iso country level
#' @param cells switch between 59199 ("magpiecell") and 67420 ("lpjcell") cells
#' @param absolute switch declaring the values as absolute (TRUE) or relative (FALSE)
#' for additional (type-specific) diagnostic information. If not defined (NULL) additional diagnostics
#' will not be shown.
#' @return return selected input data
#' @author Kristine Karstens, Felicitas Beier, Jan Philipp Dietrich
#'
#' @importFrom utils read.csv
#' @export


toolIso2CellCountries <- function(x, cells = "magpiecell", absolute = NULL) {
  if (cells == "magpiecell") {
    cellToCellIso <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")
    isoCellCountry <- unique(cellToCellIso$iso)
  } else if (cells == "lpjcell") {
    map <- toolGetMappingCoord2Country()
    isoCellCountry <- unique(map$iso)
  }
  y <- x[isoCellCountry, , ]

  if (isTRUE(absolute)) {
    total <- dimSums(dimSums(x, dim = c(2, 3)), dim = 1)
    returned <- dimSums(dimSums(y, dim = c(2, 3)), dim = 1)
    lost <- round((1 - (returned / total)) * 100, 2)
    if (lost != 0) vcat(1, paste0(lost, "% of the summed up values of the data set got lost."))
  }

  return(y)
}
