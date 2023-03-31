#' @title correctLPJmL_new
#' @description Convert LPJmL content (dummy function)
#' @param x magpie object provided by the read function
#'
#' @author Kristine Karstens
#' @seealso
#' [readLPJmL()]
#' @examples
#' \dontrun{
#' readSource("LPJmL", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace
#'

correctLPJmL_new <- function(x) { # nolint: object_name_linter.

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)

  return(x)
}
