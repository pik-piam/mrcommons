#' @title calcMulticropping 
#' @description calculates the ratio between area harvested and physical cropland area. Can be larger or smaller, depending on fallow land and double cropping.
#'
#' @param extend_future  if TRUE
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, David Chen
#' @seealso
#' \code{\link{calcFAOLand}},
#' \code{\link{calcCroparea}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("")
#' }
#' 
calcMulticropping <- function(extend_future=FALSE) {
  
  # kcr <- findset("kcr")
  # newproducts<-c("betr","begr")
  # kcr_red<-setdiff(kcr,newproducts)
 
  #6620  = (6620|Arable land and Permanent crops or  6620|Cropland)
  phys <- collapseNames(calcOutput("FAOLand", aggregate=FALSE)[,,"6620", pmatch=TRUE])
  area <- collapseNames(dimSums(calcOutput("Croparea", physical=FALSE, aggregate=FALSE, sectoral="kcr"),dim=3.1))
  phys <- phys[,intersect(getYears(phys),getYears(area)),]
  area <- area[,intersect(getYears(phys),getYears(area)),]

  multi <- area/phys
  multi[is.na(multi)]<-0
  multi[multi==Inf]<-0
  

  if (extend_future==TRUE){
    multi <- toolHoldConstantBeyondEnd(multi)
    phys  <- toolHoldConstantBeyondEnd(phys)
  }
  
  return(list(x=multi,
              weight=phys,
              unit="area harvested per physical area (ratio)",
              description="values above one indicate multicropping, below one fallow land"
  ))
}