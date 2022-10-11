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
  getSets(x, fulldim = FALSE)[1] <- "x.y.iso"
  out   <- toolCoord2Isocell(x, cells = cells)
  return(toolSum2Country(out))
}
