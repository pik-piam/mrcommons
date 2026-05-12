#' @title calcGrowingStockPlantations
#' @description
#' Calculates the growing stocks from FAO data for planted forests (plantations).
#'
#' @param indicator Either "per_ha" (m3/ha, area-weighted) or "total" (Mm3, absolute).
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("GrowingStockPlantations", indicator = "per_ha", aggregate = TRUE)
#' calcOutput("GrowingStockPlantations", indicator = "total", aggregate = TRUE)
#' }
#'
#' @importFrom magpiesets FRAnames
#' @export

calcGrowingStockPlantations <- function(indicator = "per_ha") {
  out <- readSource("FRA2025", subtype = "growing_stock", convert = TRUE)

  if (indicator == "per_ha") {
    x <- out[, , grep(pattern = "gs_ha_", x = getNames(out), value = TRUE)]
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

    x <- xPlant
    weight <- wPlant
    unit <- "m3/ha"
  } else if (indicator == "total") {
    x <- out[, , grep(pattern = "gs_tot_", x = getNames(out), value = TRUE)]
    getNames(x) <- gsub(pattern = "gs_tot_", replacement = "", x = getNames(x))
    getNames(x) <- FRAnames(getNames(x))

    xPlant <- setYears(collapseNames(x[, "y2025", "Plantations"]), NULL)
    xPlanted <- setYears(collapseNames(x[, "y2025", "Managed Forest"]), NULL)

    # Fill missing plantation data with planted forest as fallback
    missing <- which(xPlant == 0 & xPlanted > 0)
    xPlant[missing] <- xPlanted[missing]

    x <- xPlant
    weight <- NULL
    unit <- "Mm3"
  } else {
    stop("Invalid indicator. Use 'per_ha' or 'total'.")
  }

  return(list(x = x,
              weight = weight,
              min = 0,
              unit = unit,
              description = "Calculates Growing stocks in plantations as reported by 
              Forest Resources Assessment Data 2025."))
}
