#' @title readSiebert
#' @description Read area equipped for irrigation for the period 1997-2002 from Siebert et al.; 2007 ;
#'              Global Map of Irrigation Areas version 4.0.1
#'
#' @return magpie object in cellular resolution
#' @author David Chen
#' @examples
#'
#' \dontrun{
#'   readSource("Siebert", convert="onlycorrect")
#' }

readSiebert <- function(){

  x <- read.magpie("avl_irrig_0.5.mz")

  return(x)
}
