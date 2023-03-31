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
  plantedArea <- readSource("FRA2020", "forest_area")[, , "plantedForest"]
  plantedArea <- time_interpolate(dataset = plantedArea, interpolated_year = "y1995",
                                  integrate_interpolated_years = TRUE, extrapolation_type = "linear")
  ## VegC
  cellvegc <- calcOutput("LPJmL_new", version = "LPJmL4_for_MAgPIE_44ac93de", climatetype = "GSWP3-W5E5:historical",
                         subtype = "vegc", stage = "smoothed", aggregate = FALSE)[, "y1995", ]
  # reduce to 59199 cells and rename cells
  cellvegc <- toolCoord2Isocell(cellvegc)

  distShare <- NULL
  for (reg in getItems(cellvegc, dim = 1)) {
    temp <- cellvegc[reg, , ] / dimSums(cellvegc[reg, , ], dim = 1)
    distShare <- mbind(distShare, temp)
  }
  plantedCell <- plantedArea[getItems(distShare, dim = 1), , ] * distShare
  out <- setYears(plantedCell[, "y1995", ], NULL)

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
