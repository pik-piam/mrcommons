#' toolAggregateTemporalStock
#'
#' Aggregates a magclass object to temporal resolution assumed for stock variables in models like REMIND
#' (roughly speaking 5-year timesteps representing the average of the years x-2..x+2)
#'
#' @param x magclass object
#' @author Falk Benke

toolAggregateTemporalStock <- function(x) {

  allTs <- toolGetMapping("temporalMappingStockVars.csv", where = "mrcommons", type = "temporal")

  ts <- allTs[allTs$period %in% getYears(x) & allTs$year %in% getYears(x), ]

  weight <- new.magpie(years = unique(ts$year), fill = 1)

  x <- toolAggregate(x, dim = 2, rel = ts, from = "year", to = "period",
                     weight = weight, verbosity = 0)

  return(x)
}
