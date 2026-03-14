#' @title calcGrowingStockPlantations
#' @description
#' Calculates the growing stocks on FAO data.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @examples
#' \dontrun{
#' calcOutput("GrowingStockPlantations", aggregate = TRUE)
#' }
#'
#' @importFrom magpiesets FRAnames
#' @export

calcGrowingStockPlantations <- function() {
  ## Read Growing Stock
  out <- readSource("FRA2025", subtype = "growing_stock", convert = TRUE)
  x <- out[, , grep(pattern = "ha", x = getNames(out), value = TRUE)]
  getNames(x) <- gsub(pattern = "gs_ha_", replacement = "", x = getNames(x))
  getNames(x) <- FRAnames(getNames(x))
  area <- readSource("FRA2025", "forest_area", convert = TRUE)
  getNames(area) <- FRAnames(getNames(area))
  vars <- intersect(getNames(area), getNames(x))
  x <- x[, , vars]
  weight <- area[, , vars] + 10^-10

  xPlant <- setYears(collapseNames(x[, "y2025", "Plantations"]), NULL)
  wPlant <- setYears(collapseNames(weight[, "y2025", "Plantations"]), NULL)
  xPlanted <- setYears(collapseNames(x[, "y2025", "Managed Forest"]), NULL)
  wPlanted <- setYears(collapseNames(weight[, "y2025", "Managed Forest"]), NULL)

  # Fill missing plantation data with planted forest as fallback
  missing <- which(xPlant == 0 & xPlanted > 0)
  xPlant[missing] <- xPlanted[missing]
  wPlant[missing] <- wPlanted[missing]

  return(list(x = xPlant,
              weight = wPlant,
              min = 0,
              unit = "m3/ha",
              description = "Calculates Growing stocks as reported by Forest Resources Assessment Data 2025."))

}
