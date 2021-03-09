#' @title correctLUH2v2
#' @description Correct LUH2v2 content
#'
#' @param x magpie object provided by the read function
#' @param subtype switch between different inputs
#' 
#' @return List of magpie object with results on cellular level
#' 
#' @author Florian Humpenoeder, Stephen Wirth, Kristine Karstens, Felicitas Beier, Jan Philipp Dietrich
#' 
#' @importFrom magclass getCells
#' 
correctLUH2v2 <- function(x, subtype) {
  
  if (any(is.na(x))) {
    vcat(verbosity=1, paste(sum(is.na(x))/length(x)*100,"% of data points with NAs in LUH2. set to 0."))
    x[is.na(x)]<-0
  }
  if (any(x<0)) {
    vcat(verbosity=1, paste(sum(x<0)/length(x)*100,"% of data points with negative values in LUH2. set to 0."))
    x[x<0]<-0
  }

  years <- getYears(x, as.integer=TRUE)
  
  if (grepl("states",subtype) & length(intersect(2001:2015,years))>0 & 2000%in%years & 2005%in%years) {
    
    # check, if in JPN pasture+rangeland is unnaturally low
    if (sum(x["JPN","y2005",c("pastr","range")])<0.01) {
      
      #if so correct all years since 2001 (first year of buggy data)
      #using secondary forest area as buffer 
      bugged_years <- intersect(2001:2015, years)
      pasture      <- setYears(x["JPN","y2000",c("pastr","range")],NULL) 
      x["JPN",bugged_years,"secdf"]            <- x["JPN",bugged_years,"secdf"] - dimSums(pasture,dim=3)
      x["JPN",bugged_years,c("pastr","range")] <- x["JPN",bugged_years,c("pastr","range")] + setYears(pasture,NULL)
      
      #correct for negative values (first in pasture than rangelands), if secondary forest is exceeded
      x["JPN",bugged_years,"pastr"][x["JPN",bugged_years,"secdf"] < 0] <- x["JPN",bugged_years,"pastr"][x["JPN",bugged_years,"secdf"] < 0] + x["JPN",bugged_years,"secdf"][x["JPN",bugged_years,"secdf"] < 0]
      x["JPN",bugged_years,"secdf"][x["JPN",bugged_years,"secdf"] < 0] <- 0
      
      x["JPN",bugged_years,"range"][x["JPN",bugged_years,"pastr"] < 0] <- x["JPN",bugged_years,"range"][x["JPN",bugged_years,"pastr"] < 0] + x["JPN",bugged_years,"pastr"][x["JPN",bugged_years,"pastr"] < 0]
      x["JPN",bugged_years,"pastr"][x["JPN",bugged_years,"pastr"] < 0] <- 0
      x["JPN",bugged_years,"range"][x["JPN",bugged_years,"range"] < 0] <- 0
      
    } else {stop("it seems the Japan bug in LUH2v2 has been removed. Please remove the bugfix in correct LUH2v2 before proceeding!")}
  }
  
  return(x)
}  