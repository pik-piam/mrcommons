#' @title correctAndrijevic2019
#' @description correct Andrijevic governance data
#' @return corrected magpie object iso-level
#'
#' @param x magpie object provided by the read function
#' @param subtype data to be returned:
#'                "historical" for observed data until 2015
#'                "projected" for projected SSP scenario data from 2015 to 2099
#'
#' @author Felicitas Beier
#' @seealso
#' [readAndrijevic2019()]
#'
correctAndrijevic2019 <- function(x, subtype) {

  x <- toolCountryFill(x, fill = NA)

  # fill NA values using population of year 2010 as weight
  pop        <- calcOutput("Population", scenario = "SSP2", years = 2010, aggregate = FALSE)
  pop2       <- new.magpie(cells_and_regions = getCells(x), years = getYears(x))
  pop2[, , ] <- pop

  z <- NULL
  for (i in getNames(x)) {
    y <- toolFillWithRegionAvg(x[, , i], weight = pop2, verbose = FALSE, warningThreshold = 1, noteThreshold = 0.5)
    z <- mbind(z, y)
  }

  z
}
