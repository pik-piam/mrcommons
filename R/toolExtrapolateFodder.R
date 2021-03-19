#' @title       toolExtrapolateFodder
#' @description Extrapolate fodder data, based on two time steps (5-averages around this years)
#' 
#' @param exyears two years 
#'
#' @return magpie object including extrapolated years
#' @author Kristine Karstens
#' 
#' @export

toolExtrapolateFodder <- function(x, exyears=c(2004, 2009), average=5) {
  
  dt  <- floor(average/2)
  tmp <- time_interpolate(mbind(toolTimeAverage(x[,seq(exyears[1]-dt,exyears[1]+dt),],average), 
                                 toolTimeAverage(x[,seq(exyears[2]-dt,exyears[2]+dt),],average)),
                           c(2012:2015), extrapolation_type = "linear")
  
  tmp <- toolConditionalReplace(tmp, "<0", 0)
  x   <- mbind(x, tmp)
  
  return(x)
}
