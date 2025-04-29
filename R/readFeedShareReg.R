#' Read in regression coefficients for central feed shares
#'
#' @description
#' Read in csv file containing coefficients of non-linear regression analysis
#' for the calculation of future central feed shares in feed baskets dependent
#' on livestock productivity trends
#'
#' @return MAgPIE object containing regression coefficients
#' @author Isabelle Weindl
#' @seealso [madrat::readSource()]
#' @export
#'
#' @examples
#' \dontrun{
#' a <- readSource("FeedShareReg")
#' }
#' @importFrom magclass read.magpie
readFeedShareReg <- function() {
  file <-  "DM_feed_shr_regression_nls.csv"
  feedShareReg <- read.magpie(file)
  return(feedShareReg)

}
