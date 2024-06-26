#' @title calcGrowingStockpha
#' @description
#' Calculates the growing stocks on FAO data.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @examples
#' \dontrun{
#' calcOutput("GrowingStockpha", aggregate = TRUE)
#' }
#'
#' @importFrom magpiesets FRAnames
#' @export

calcGrowingStockpha <- function() {
  ## Read Growing Stock
  out <- readSource("FRA2020", subtype = "growing_stock", convert = TRUE)
  x <- out[, , grep(pattern = "ha", x = getNames(out), value = TRUE)]
  getNames(x) <- gsub(pattern = "gs_ha_", replacement = "", x = getNames(x))
  getNames(x) <- FRAnames(getNames(x))
  area <- readSource("FRA2020", "forest_area", convert = TRUE)
  getNames(area) <- FRAnames(getNames(area))
  vars <- intersect(getNames(area), getNames(x))
  x <- x[, , vars]
  weight <- area[, , vars] + 10^-10

  x <- setYears(collapseNames(x[, "y2000", "Plantations"]), NULL)
  weight <- setYears(collapseNames(weight[, "y2000", "Plantations"]), NULL)

  return(list(x = x,
              weight = weight,
              min = 0,
              unit = "m3/ha",
              description = "Calculates Growing stocks as reported by Forest Resources Assessment Data 2020."))

}
