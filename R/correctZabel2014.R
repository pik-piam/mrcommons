#' @title correctZabel2014
#' @description correct Zabel crop suitability data
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author Patrick v. Jeetze
#' @seealso
#'   \code{\link{readZabel2014}}
#' @examples
#' \dontrun{
#' readSource("Zabel2014", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctZabel2014 <- function(x) {

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)

  return(x)
}
