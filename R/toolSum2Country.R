#' toolSum2Country
#'
#' Efficient method to sum cellular data with country dimension as first
#' sub-dimension to country level
#' @param x magpie object on cellular level with countries in dim 1.1
#' @return return selected input data on ISO country level
#' @author Jan Philipp Dietrich
#' @export

toolSum2Country <- function(x) {
  map <- data.frame(from = getItems(x, dim = 1),
                    to = getItems(x, dim = 1.1, full = TRUE))
  x <- toolAggregate(x, map)
  getSets(x, fulldim = FALSE)[1] <- "country"
  return(toolCountryFill(x, fill = 0, verbosity = 2))
}
