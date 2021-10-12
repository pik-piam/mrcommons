convertEDGAR6 <- function(x,subtype) {
  # fill all missing countries with 0
  x <- toolCountryFill(x,fill=0)
  return(x)
}  