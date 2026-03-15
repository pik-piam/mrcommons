#' @title calcGrowingStockNRF
#' @description
#' Calculates the growing stocks from FAO data for naturally regenerating forests
#' i.e. primary forests and secondary forests.
#'
#' @param indicator Either "per_ha" (m3/ha, area-weighted) or "total" (Mm3, absolute).
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("GrowingStockNRF", indicator = "per_ha", aggregate = TRUE)
#' calcOutput("GrowingStockNRF", indicator = "total", aggregate = TRUE)
#' }
#'
#' @importFrom magpiesets FRAnames
#' @export

calcGrowingStockNRF <- function(indicator = "per_ha") {
  out <- readSource("FRA2025", subtype = "growing_stock", convert = TRUE)

  if (indicator == "per_ha") {
    x <- out[, , grep(pattern = "gs_ha_", x = getNames(out), value = TRUE)]
    getNames(x) <- gsub(pattern = "gs_ha_", replacement = "", x = getNames(x))
    getNames(x) <- FRAnames(getNames(x))
    area <- readSource("FRA2025", "forest_area", convert = TRUE)
    getNames(area) <- FRAnames(getNames(area))
    vars <- intersect(getNames(area), getNames(x))
    x <- x[, , vars]
    weight <- area[, , vars]

    x <- setYears(collapseNames(x[, "y2025", "Natural Forest"]), NULL)
    weight <- setYears(collapseNames(weight[, "y2025", "Natural Forest"]), NULL)
    unit <- "m3/ha"
  } else if (indicator == "total") {
    x <- out[, , grep(pattern = "gs_tot_", x = getNames(out), value = TRUE)]
    getNames(x) <- gsub(pattern = "gs_tot_", replacement = "", x = getNames(x))
    getNames(x) <- FRAnames(getNames(x))
    x <- setYears(collapseNames(x[, "y2025", "Natural Forest"]), NULL)
    weight <- NULL
    unit <- "Mm3"
  } else {
    stop("Invalid indicator. Use 'per_ha' or 'total'.")
  }

  return(list(
    x = x,
    weight = weight,
    min = 0,
    unit = unit,
    description = paste("Calculates Growing stocks in naturally regenerating forests",
                        "as reported by Forest Resources Assessment Data 2025.")
  ))
}
