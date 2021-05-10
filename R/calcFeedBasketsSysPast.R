#' Calculate historical system-specific feed baskets based on output of MAgPIE_FEED model
#' as DM feed biomass (different types of feed) needed per DM livestock products of respective systems
#' 
#' 
#' @return Historical system-specific feed baskets and corresonding weights as a list of two MAgPIE
#' objects
#' @author Isabelle Weindl, Benjamin Bodirsky, Jan Philipp Dietrich
#' @seealso \code{\link{calcOutput}}, \code{\link{readFeedModel}}, \code{\link{calcFeedBasketsPast}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FeedBasketsSysPast")
#' 
#' }
#' @importFrom magclass getNames
#' @importFrom luscale rename_dimnames

calcFeedBasketsSysPast <- function() {

  #read in system-specific feed basket data (for sys_dairy,sys_beef,sys_pig,sys_hen,sys_chicken)
  fbask_sys <-  readSource(type="FeedModel",subtype="FeedBaskets")
  
  #Interpolate for yearly data
  
  
  
  #expand dim=3.2 to kall (add products like wood and woodfuel)
  kdiff         <- setdiff(findset("kall"),getNames(fbask_sys,dim=2))
  fbask_sys          <- add_columns(fbask_sys,addnm = kdiff,dim=3.2)
  fbask_sys[,,kdiff] <- 0
  
  #use livestock production as weight
  kli<-findset("kli")
  massbalance<-calcOutput("FAOmassbalance_pre",aggregate = F)
  weight <- collapseNames(massbalance[,,kli][,,"dm"][,,"production"])
  
  mapping<-data.frame(
    kli=c( "livst_pig","livst_rum","livst_chick","livst_egg","livst_milk"),
    sys=c("sys_pig","sys_beef","sys_chicken","sys_hen","sys_dairy" ),
    stringsAsFactors = FALSE)
  
  weight<-rename_dimnames(weight,dim = 3,query = mapping,from = "kli", to="sys")
  
  #interpolate for yearly data, keeping ends constant to not have negatives
  iyears <- getYears(weight)
  fbask_sys <- time_interpolate(fbask_sys, interpolated_year = iyears, integrate_interpolated_years = TRUE)
  fbask_sys[,c(1961:1964),] <- setYears(fbask_sys[,1965,],NULL)
  fbask_sys[,c(2011:getYears(fbask_sys, as.integer = TRUE)[length(getYears(fbask_sys))]),] <- setYears(fbask_sys[,2010,],NULL)
  
  
  # remove datasets with NAs in weight/data
  fbask_sys<-toolNAreplace(x=fbask_sys,weight=weight,replaceby=0)
  weight=fbask_sys$weight
  out=fbask_sys$x
  
  
  return(list(x=out,weight=weight,
              unit="1",
              description="Detailed historical system-specific feed requirements in DM per DM products generated for 5 livestock commodities."))
}