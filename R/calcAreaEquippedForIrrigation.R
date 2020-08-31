#' @title calcAreaEquippedForIrrigation
#' @description Calculates the area equipped for irrigation based on LU2v2 or other dataset. 
#' For LUH2v2, it assumes, that all cropland irrigated in the last 20 years at least once is equipped for irrigation.
#'
#' @param cellular if true, dataset is returned on 0.5 degree resolution
#' @param source switch between different data sources
#' @param selectyears default on "past"
#' 
#' @return List of magpie objects with results on country/cellular level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @seealso
#' \code{\link{calcLanduseInitialisation}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("AreaEquippedForIrrigation", source="LUH2v2", cellular=TRUE, aggregate=FALSE)
#' }
#' @importFrom magclass as.magpie getRegionList<- ncells
#' @importFrom luscale groupAggregate


calcAreaEquippedForIrrigation<-function(cellular=FALSE, source="LUH2v2", selectyears="past"){
  
  selectyears <- sort(findset(selectyears,noset = "original"))
 
  if(source=="LUH2v2"){
    
    years_needed <- as.integer(substring(selectyears,2))
    years_needed <- (years_needed[1]-20):tail(years_needed,1)
    
    x     <- calcOutput("LUH2v2", landuse_types="magpie", irrigation=TRUE, cellular=TRUE, selectyears=years_needed, aggregate=FALSE)[,,"irrigated"]
    x     <- dimSums(x,dim=3)
    years <- as.numeric(substring(selectyears,2))
    out   <- NULL
    
    for (year_x in years){
      span <- (year_x-20):year_x
      tmp  <- setYears(as.magpie(apply(X = x[,span,], FUN = max, MARGIN = 1)), paste0("y",year_x))
      out  <- mbind(out,tmp)
    }
    
  } else if (source=="Siebert"){
    
    out   <- readSource("Siebert", convert="onlycorrect")
    
  } else stop("Unknown source for calcAreaEquippedForIrrigation")
  
  
  if (!cellular){
    mapping <- toolGetMapping(type="cell", name="CountryToCellMapping.csv")
    out     <- toolAggregate(out, rel=mapping, from="celliso", to="iso", dim=1)
    out     <- toolCountryFill(out, fill=0)
  }
  
  return(list(
    x=out,
    weight=NULL,
    unit="Million ha",
    description="Area equpped for irrigation in million hectare.",
    isocountries=!cellular))
}
