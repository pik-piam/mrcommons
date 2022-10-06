#' toolConv2CountryByCelltype
#'
#' Aggregates cellular data to ISO country level after conversion of cellular
#' data to a specific cell setup (this type is relevant as some settings,
#' such as "magpiecell" remove some cells and therby affect country sums)
#' @param x magpie object on cellular level
#' @param cells switch between 59199 ("magpiecell") and 67420 ("lpjcell") cells
#' @return return selected input data on ISO country level
#' @author Jan Philipp Dietrich

toolConv2CountryByCelltype <- function(x, cells) {
  out   <- toolCoord2Isocell(x, cells = cells)
  map <- data.frame(from = getItems(out, dim = 1),
                    to = getItems(out, dim = 1.1, full = TRUE))
  out <- toolAggregate(out, map)
  getSets(out, fulldim = FALSE)[1] <- "country"
  return(toolCountryFill(out, fill = 0, verbosity = 2))
}
