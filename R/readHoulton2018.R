#' @title readHoulton2018
#' @description Read Houlton 2018 model on weathering of N from rocks
#'
#' @return magpie object global resolution by landuse types in Mt of N weathered from rocks each year
#' @author David M Chen
#' @examples
#'
#' \dontrun{
#'   readSource("Houlton2018", convert=FALSE)
#' }
#' @importFrom raster raster resample crs area
#' @export

readHoulton2018 <- function(){

rockn <- raster("Ncw_BestEst2_Model1A_longlat_lr-bil_kgkm2.nc")
names(rockn) <- gsub("\\.\\..|\\.", "_", names(rockn))

mag_ext <- extent(-180, 180, -90, 90)
mag_res <- 0.5
mag_r <- raster(resolution=mag_res, ext=mag_ext, crs=crs(rockn))

rockn <- resample(rockn,mag_r, "ngb" )

#convert from kg/km2 to kg
rockn <- rockn*area(rockn)
#covnert to Mt
rockn <- rockn/10^9

out <- as.magpie(rockn)

#some magpie coords missing from rockn .nc, fill with0s
map <- toolGetMappingCoord2Country()
missing_cells <- setdiff(map$coords, getItems(out, dim=1))
fill <- new.magpie(cells_and_regions = missing_cells, years=getYears(out), names=getNames(out),  fill=0)
out <- mbind(out, fill)

#aggregate to global  scale based on LUH
out <- toolCoord2Isocell(out)

landuse <- calcOutput("LanduseInitialisation", cellular=TRUE, nclasses="six", fao_corr=TRUE, selectyears="past", input_magpie=FALSE, aggregate=F)
landuse_shr <- landuse/dimSums(landuse,dim=3)

#make past years - hold constant
getYears(out) <- 1965
out <- time_interpolate(out, interpolated_year=getYears(landuse), integrate_interpolated_years = TRUE)

#split among landuse shares
out <- out*landuse_shr
#aggregate
out <- dimSums(out, dim=1,na.rm=T)

return(out)


}
