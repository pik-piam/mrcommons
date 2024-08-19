#' @title calcManureFuelShr
#' @description calculates the share of Manure excreted during grazing which is collected for fuel.
#' For the future, we assume that with the development, the fuel share reaches 0.
#'
#' @param products IPCC: IPCC products. MAgPIE: Magpie products
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcExcretion()]
#' @examples
#' \dontrun{
#' calcOutput("ManureFuelShr")
#' }
#'
calcManureFuelShr <- function(products = "magpie") {

  excretion         <- calcOutput("ExcretionIPCC", aggregate = FALSE, products = products)
  pastureCategories <- c("pasture_range_paddock", "fuel")
  excretion         <- excretion[, , pastureCategories]
  weight            <- dimSums(excretion, dim = 3.2)
  manureFuelShr     <- collapseNames(excretion[, , "fuel"] / weight)

  weight[is.na(weight)]               <- 0
  manureFuelShr[is.na(manureFuelShr)] <- 0

  manureFuelShr <- toolHoldConstantBeyondEnd(manureFuelShr)
  weight        <- toolHoldConstantBeyondEnd(weight)

  tmp <- ds <- calcOutput("DevelopmentState", aggregate = FALSE)
  tmp[, , ] <- 1
  manureFuelShr <- convergence(origin = manureFuelShr * tmp, aim = manureFuelShr * (1 - ds),
                               start_year = "y2010", end_year = "y2050", type = "s")
  weight <- weight * tmp + 10^-10

  return(list(x           = manureFuelShr,
              weight      = weight,
              unit        = "share",
              description = "share of excreted nitrogen on pastures that is collected for fuel",
              min         = 0,
              max         = 1))
}
