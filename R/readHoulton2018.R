#' @title readHoulton2018
#' @description Read Houlton 2018 model on weathering of N from rocks
#'
#' @return magpie object global resolution by landuse types in Mt of N weathered from rocks each year
#' @author David M Chen
#' @examples
#' \dontrun{
#' readSource("Houlton2018", convert = FALSE)
#' }
#' @importFrom raster raster resample crs area extent
#' @export

readHoulton2018 <- function() {

  rockn <- raster("Ncw_BestEst2_Model1A_longlat_lr-bil_kgkm2.nc")
  names(rockn) <- gsub("\\.\\..|\\.", "_", names(rockn))
  rocknNames <- names(rockn)

  magExt <- extent(-180, 180, -90, 90)
  magRes <- 0.5
  magR <- raster(resolution = magRes, ext = magExt, crs = crs(rockn))

  rockn <- resample(rockn, magR, "ngb")

  # convert from kg/km2 to kg
  rockn <- rockn * area(rockn)
  # convert to Mt
  rockn <- rockn / 10^9
  names(rockn) <- rocknNames
  out <- as.magpie(rockn)

  # some magpie coords missing from rockn .nc, fill with0s
  map <- toolGetMappingCoord2Country()
  missingCells <- setdiff(map$coords, getItems(out, dim = 1))
  fill <- new.magpie(cells_and_regions = missingCells, years = getYears(out), names = getNames(out),  fill = 0)
  out <- mbind(out, fill)

  return(out)
}
