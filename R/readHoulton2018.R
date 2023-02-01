#' @title readHoulton2018
#' @description Read Houlton 2018 model on weathering of N from rocks
#'
#' @return magpie object global resolution by landuse types in Mt of N weathered from rocks each year
#' @author David M Chen
#' @examples
#' \dontrun{
#' readSource("Houlton2018", convert = FALSE)
#' }
#' @importFrom raster raster resample crs area
#' @export

readHoulton2018 <- function() {

rockn <- raster("Ncw_BestEst2_Model1A_longlat_lr-bil_kgkm2.nc")
names(rockn) <- gsub("\\.\\..|\\.", "_", names(rockn))
rockn_name <- names(rockn)

mag_ext <- extent(-180, 180, -90, 90)
mag_res <- 0.5
mag_r <- raster(resolution = mag_res, ext = mag_ext, crs = crs(rockn))

rockn <- resample(rockn, mag_r, "ngb")

# convert from kg/km2 to kg
rockn <- rockn * area(rockn)
# covnert to Mt
rockn <- rockn / 10^9
names(rockn) <- rockn_name
out <- as.magpie(rockn)

# some magpie coords missing from rockn .nc, fill with0s
map <- toolGetMappingCoord2Country()
missing_cells <- setdiff(map$coords, getItems(out, dim = 1))
fill <- new.magpie(cells_and_regions = missing_cells, years = getYears(out), names = getNames(out),  fill = 0)
out <- mbind(out, fill)


return(out)


}
