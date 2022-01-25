#' @title calcIFPRIsubsidy
#' @description Adds non-allocated subsidies to crop subsidies (as most subsidies are linked to land area), and
#' excludes NRP subsidies (as those are border measures, which are already reflected in ag. prices)
#' @return magpie object. in mio. USDMER05
#' @author Debbora Leip
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("IFPRIsubsidy")
#' }
#'
calcIFPRIsubsidy <- function() {

  subsidies <- dimSums(readSource("IFPRIsubsidy")[, , "NRP", invert = TRUE], dim = 3.2) / 1e6

  # add non-allocated subsidies to crop subsidies (as most subsidies are directly payments linked to land)
  subsidies[, , "Crops"] <- dimSums(subsidies[, , c("Crops", "Non-Allocated")], dim = 3)
  subsidies <- subsidies[, , "Non-Allocated", invert = TRUE]

  return(list(x = subsidies,
              weight = NULL,
              unit = "mio. USDMER05",
              description = "Agricultural subsidies"))
}
