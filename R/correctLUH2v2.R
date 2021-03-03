#' @importFrom magclass getCells<- getCells
correctLUH2v2<-function(x,subtype){
  
  if(any(is.na(x))){
    vcat(verbosity=1, paste(sum(is.na(x))/length(x)*100,"% of data points with NAs in LUH2. set to 0."))
    x[is.na(x)]<-0
  }
  if(any(x<0)){
    vcat(verbosity=1, paste(sum(x<0)/length(x)*100,"% of data points with negative values in LUH2. set to 0."))
    x[x<0]<-0
  }

  if (length(getCells(x))==59199) {
    #rename old "AFR.1"-style in new "GLO.1"-style
    getCells(x) <- paste0("GLO",substring(getCells(x),4))
    x     <- toolCell2isoCell(x)
    years <- getYears(x, as.integer=TRUE)
    if(grepl("states",subtype) & length(intersect(2001:2015,years)>0) & 2000%in%years & 2005%in%years){
      if(sum(x["JPN","y2005",c("pastr","range")])<0.01) {
        pasture <- x["JPN","y2000",c("pastr","range")]
        bugged_years <- intersect(2001:2015,years)
        x["JPN",bugged_years,"secdf"]=x["JPN",bugged_years,"secdf"] - setYears(dimSums(pasture,dim=3),NULL)
        x["JPN",bugged_years,c("pastr","range")]=x["JPN",bugged_years,c("pastr","range")] + setYears(pasture,NULL)
      } else {stop("it seems the Japan bug in LUH2v2 has been removed. Please remove the bugfix in correct LUH2v2 before proceeding!")}
    }
  } else if (length(getCells(x))==67420) {
    # rename to new cell name standard x.y.iso
    x <- addLocation(x)
    x <- collapseDim(x, dim=c("N", "cell"))
    map         <- toolGetMappingCoord2Country()
    x           <- x[map$coords,,]
    getCells(x) <- paste(map$coords, map$iso, sep=".") 
    names(dimnames(x))[1] <- "x.y.iso"
  }
  
  return(x)
}  