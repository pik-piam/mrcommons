#' @title readLi2020
#' @description Read lignocellulosic bioenergy crop yield maps from Li et al. (2020),
#' doi:10.5194/essd-12-789-2020. Random-forest upscaling of field observations using
#' climate and soil conditions. Yields in t DM ha-1 yr-1, ~year 2010.
#'
#' @return MAgPIE object on cellular level with yields for Eucalypt, Miscanthus, Poplar,
#' Switchgrass, Willow, Best_crop and Best_crop_type.
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#' a <- readSource("Li2020")
#' }
#' @importFrom terra rast extract
readLi2020 <- function() {

  # Load celliso mapping for 67420 MAgPIE grid cells
  map <- toolGetMappingCoord2Country(pretty = TRUE)

  # Read all 7 variables from NC file as a multi-layer SpatRaster
  r <- terra::rast("Bioenergy_crop_yields.nc")

  # Extract values at MAgPIE cell coordinates (drop terra's ID column)
  vals <- terra::extract(r, map[c("lon", "lat")])[, -1]

  mag <- as.magpie(vals, spatial = 1)

  getNames(mag) <- names(vals)
  getCells(mag) <- paste(map$coords, map$iso, sep = ".")
  getYears(mag) <- "y2010"
  getSets(mag)  <- c("x.y.iso", "t", "data")

  return(mag)
}
