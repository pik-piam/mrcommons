#' @title calcIFPRIsubsidy
#' @description Adds non-allocated subsidies to crop subsidies (as most subsidies are linked to land area), and
#' excludes NRP subsidies (as those are border measures, which are already reflected in ag. prices)
#' @param fillGaps boolean, should gaps in the dataset be filled using interpolation?
#' @return magpie object. in mio. USDMER05
#' @author Debbora Leip
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("IFPRIsubsidy")
#' }
#'
calcIFPRIsubsidy <- function(fillGaps = TRUE) {

  subsidies <- dimSums(readSource("IFPRIsubsidy")[, , "NRP", invert = TRUE], dim = 3.2) / 1e6

  # add non-allocated subsidies to crop subsidies (as most subsidies are directly payments linked to land)
  subsidies[, , "Crops"] <- dimSums(subsidies[, , c("Crops", "Non-Allocated")], dim = 3)
  subsidies <- subsidies[, , "Non-Allocated", invert = TRUE]

  # interpolate gaps for each country
  if (isTRUE(fillGaps)) {
    for (ctry in getItems(subsidies, dim = 1)) {
      years <- where(subsidies[ctry, , ] != 0)$true$years
      yGaps <- setdiff(getItems(subsidies, dim = 2), years)
      if (length(yGaps) > 0 && length(years) > 0) {
        subsidies[ctry, , ] <- time_interpolate(subsidies[ctry, years, ],
                                                interpolated_year = yGaps,
                                                integrate_interpolated_years = TRUE,
                                                extrapolation_type = "constant")
      }
    }
  }

  return(list(x = subsidies,
              weight = NULL,
              unit = "mio. USDMER05",
              description = "Agricultural subsidies"))
}
