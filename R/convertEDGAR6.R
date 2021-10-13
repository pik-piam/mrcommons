convertEDGAR6 <- function(x,subtype) {
  # fill all missing countries with 0
  getRegions(x) = sub(pattern = "ANT",replacement = "SXM",x = getRegions(x))
  getRegions(x) = sub(pattern = "SCG",replacement = "SRB",x = getRegions(x))
  x <- toolCountryFill(x,fill=0)
  return(x)
}
