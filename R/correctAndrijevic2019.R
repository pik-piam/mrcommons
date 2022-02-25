#' @title correctAndrijevic2019
#' @description correct Andrijevic governance data
#' @return corrected magpie object iso-level
#'
#' @param x magpie object provided by the read function
#' @param subtype data to be returned:
#'                "historical" for observed data until 2015
#'                "projected" for projected SSP scenario data from 2015 to 2099
#'
#' @importFrom madrat toolCountryFill toolFillWithRegionAvg
#'
#' @author Felicitas Beier
#' @seealso
#' [readAndrijevic2019()]

correctAndrijevic2019 <- function(x, subtype) {

  x <- toolCountryFill(x, fill = NA)
  
  # fill NA values using population of year 2010 as weight
  pop           <- setYears(calcOutput("Population", aggregate = FALSE)[, 2010, 1], 
                            NULL)
  pop2          <- new.magpie(cells_and_regions = getCells(x),
                              years = getYears(x))
  pop2[, , ]    <- pop
  
  z <- NULL
  for (i in getNames(x)) {
    y <- toolFillWithRegionAvg(x[, , i], weight = pop2, verbose = FALSE,
                               warningThreshold = 1) # to include (once madrat ready): noteThreshold = 0.5, 
    z <- mbind(z, y)
  }
  
  return(z)
}
