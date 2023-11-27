#' @title correctLandInG
#' @description correct LandInG data. Convert unit from ha to mio ha
#' @return corrected magpie object
#' @param x magpie object provided by the read function
#' @author David Hoetten, Felicitas Beier
#' @seealso
#'   \code{\link{readLandInG}}
#' @examples
#' \dontrun{
#' a <- readSource("LandInG", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctLandInG <- function(x) {

  # replace NAs and negatives with 0
  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)
  # convert from ha to Mha
  x <- x * 1e-06

  return(x)

}
