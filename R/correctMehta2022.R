#' @title correctMehta2022
#' @description correct lobal Area Equipped for Irrigation Dataset 1900-2015
#'              from Mehta et al., 2022
#'
#' @param x magpie object provided by the read function
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#'
#' \dontrun{
#'   readSource("Mehta2022", convert="onlycorrect")
#' }

correctMehta2022 <- function(x) {

  x <- toolConditionalReplace(x,
                              conditions = c("is.na()", "<0"),
                              replaceby = 0)

  return(x)
}
