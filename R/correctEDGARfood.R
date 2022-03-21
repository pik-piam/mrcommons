#' @title correctEDGARfood
#' @description correct Edgar-food data
#' @return corrected magpie object
#' @param x magpie object provided by the read function
#' @param subtype Type of data that should be read
#' \itemize{
#' \item \code{foodSystemEmi}: Total food system emissions of different countries
#' \item \code{foodSystemShare}: Share of food system emissions in total emissions
#' \item \code{foodSystemSector}: Food system emissions separated by country, sector and substance
#' }
#' @author David Hoetten
#' @seealso
#'   \code{\link{readEDGARfood}}
#'
#' @importFrom madrat toolConditionalReplace
correctEDGARfood <- function(x, subtype) {
  if (subtype == "foodSystemShare") {
    x <- toolConditionalReplace(x, conditions = c("<0"), replaceby = 0)
    x <- toolConditionalReplace(x, conditions = c(">1"), replaceby = 1)
  }
  return(x)
}
