#' @title correctAndrijevic2019
#' @description correct Andrijevic governance data
#' @return corrected magpie object iso-level
#'
#' @param x magpie object provided by the read function
#' @param subtype data to be returned:
#'                "historical" for observed data until 2015
#'                "projected" for projected SSP scenario data from 2015 to 2099
#'
#' @importFrom madrat toolCountryFill
#'
#' @author Felicitas Beier
#' @seealso
#' \code{\link{readAndrijevic2019}}

correctAndrijevic2019 <- function(x, subtype) {

  x <- toolCountryFill(x, fill = NA)

  return(x)
}
