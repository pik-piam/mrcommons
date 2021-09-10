#' @title calcPlantationCellular
#' @description Calculates the cellular plantation area based on carbon densities
#'
#' @return List of magpie object with results on cellular level, weight on cellular level, unit and description.
#' @author Abhijeet Mishra
#'
#' @importFrom magclass setYears getRegions dimSums mbind
#'
#' @examples
#' \dontrun{
#' calcOutput("PlantationCellular")
#' }
#'
calcPlantationCellular <- function() {
  planted_area <- readSource("FRA2020", "forest_area")[, , "plantedForest"]
  planted_area <- time_interpolate(dataset = planted_area, interpolated_year = "y1995", integrate_interpolated_years = TRUE, extrapolation_type = "linear")
  ## VegC
  cellvegc <- calcOutput("LPJmL_new", version = "LPJmL4_for_MAgPIE_84a69edd", climatetype = "GSWP3-W5E5:historical", subtype = "vegc", stage = "smoothed", aggregate = FALSE)[, "y1995", ]
  # reduce to 59199 cells and rename cells
  cellvegc <- toolCoord2Isocell(cellvegc)

  dist_share <- NULL
  for (reg in getRegions(cellvegc)) {
    temp <- cellvegc[reg, , ] / dimSums(cellvegc[reg, , ], dim = 1)
    dist_share <- mbind(dist_share, temp)
  }
  planted_cell <- planted_area[getRegions(dist_share), , ] * dist_share
  out <- setYears(planted_cell[, "y1995", ], NULL)

  return(list(
    x = out,
    weight = NULL,
    unit = "1",
    min = 0,
    max = 1, ### 100% share
    description = "Cellular level share of plantations based on c-density",
    isocountries = FALSE
  ))
}
