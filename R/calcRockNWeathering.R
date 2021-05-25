#' @title calcRockNWeathering
#' 
#' @description calculates amount of yearly N from rock weathering by country or global total, disaggregated by land use type (LUH2v2 6 class, with FAO forest correction)
#' @return MAgPIE object of amount of N (Mt)
#' @author David M Chen



calcRockNWeathering <- function(){
x <- readSource("Houlton2018", convert=F)

#aggregate to country level 
x <- toolCoord2Isocell(x)

landuse <- calcOutput("LanduseInitialisation", cellular=TRUE, nclasses="six", fao_corr=TRUE, selectyears="past", input_magpie=FALSE, aggregate=F)
landuse_shr <- landuse/dimSums(landuse,dim=3,na.rm=T)

#make past years - hold constant
getYears(x) <- 1965
x <- time_interpolate(x, interpolated_year=getYears(landuse), integrate_interpolated_years = TRUE)

#split among landuse shares
out <- x*landuse_shr
#aggregate
out[is.na(out)] <- 0
rel  <- toolGetMapping("CountryToCellMapping.csv",type="cell")
out <- toolAggregate(out,rel=rel, from="celliso", to="iso")

return(list(x=out,
            weight=NULL,
            unit="Mt N/year",
            description="Amount of N weathered from rocks according to Houlton 2018",
            note=c("")))

}