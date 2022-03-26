#' toolAggregateCell2Country
#'
#' Aggregate cellular data (with coordinate information) to countries and perform consistency checks
#' @param x cellular magpie object with coordinates
#' @param weight aggregation weight
#' @param ... additional options forwarded to `toolCountryFill`
#' @return return country ISO level data
#' @author Jan Philipp Dietrich
#' @importFrom magclass getItems
#' @export

toolAggregateCell2Country <- function(x, weight = NULL, ...) {

  map <- toolGetMappingCoord2Country(extended = TRUE)

  unknown <- which(!(getItems(x, dim = 1) %in% map$coords))
  if (length(unknown) > 0) {
    warning(length(unknown), " entries of x could not be mapped to a country and will be ignored!")
    x <- x[-unknown, , ]
  }

  out <- toolAggregate(x, map, from = 2, partrel = TRUE, weight = weight[getItems(x, dim = 1), , ])

  # island states are NAs: will be set to 0
  out <- toolCountryFill(out, ...)

  return(out)
}
