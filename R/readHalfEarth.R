#' @title readHalfEarth
#' @description Read in Half Earth data set containing conservation area
#'              for biodiversity protection based on the Half-Earth approach
#' @param subtype Data source to be read from
#' @return MAgPIE object containing biodiveristy protection area at cellular level
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("HalfEarth", subtype = "GLOBIO4", convert = "onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass

readHalfEarth <- function(subtype = "GLOBIO4") {

  x <- read.magpie(paste0(subtype, "/globio4_halfearth_50p_5min_fraction_2050_30min.asc"))

  return(x)
}
