#' @title calcStorage
#'
#' @description calculates stock levels of magpie commodities based on FAO data.
#' FAO data only has stock variation so we assume lowest level for each product/country as 0 level
#' USDA data to be potentially included in future second data source
#'
#' @param datasource Options of the source of data, currently only FAO:  `FAO`.
#'
#' @return List with a magpie object with stock level in tonnes
#' @author David Chen
#' @examples
#' \dontrun{
#' calcOutput("Storage", datasource = "FAO")
#' }
#'
calcStorage <- function(datasource = "FAO") {
  # use mass balance here in future, IS BUGGY!

  stocks <- collapseNames(calcOutput("FAOharmonized", aggregate = FALSE)[, , "stock_variation"])
  stocks  <- stocks[, , -grep("Total", getNames(stocks))]

  # Weighting?

  ## with positive values being withdrawals:
  stocks <- -1 * stocks
  mins <- magpply(stocks, min, c(1, 3))
  stocks <- stocks - mins

  return(list(x = stocks,
              weight = NULL,
              unit = "tonnes",
              description = "stock level based on FAO harmonized stock_variation assuming lowest stock change is 0",
              isocountries = TRUE))
}
