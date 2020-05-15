#' @title correctSiebert
#' @description correctc area equipped for irrigation for the period 1997-2002 from Siebert et al.; 2007 ;
#'              Global Map of Irrigation Areas version 4.0.1
#'
#' @return magpie object in cellular resolution
#' @param x magpie object provided by the read function
#' @author David Chen
#'
#' @examples
#'
#' \dontrun{
#'   readSource("Siebert", convert="onlycorrect")
#' }

correctSiebert <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
