#' @title calcGrowingStockNRF
#' @description
#' Growing stock of naturally regenerating forest (FRA 2025) in the year 2025, the MAgPIE
#' calibration target. See \code{\link{calcGrowingStockFRA}}.
#'
#' @param indicator Either "per_ha" (m3/ha, area-weighted) or "total" (Mm3, absolute).
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra, Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("GrowingStockNRF", indicator = "per_ha", aggregate = TRUE)
#' }
#'
#' @export

calcGrowingStockNRF <- function(indicator = "per_ha") {
  out <- calcOutput("GrowingStockFRA", category = "nat_reg", indicator = indicator,
                    aggregate = FALSE, supplementary = TRUE)
  out$x <- setYears(out$x[, "y2025", ], NULL)
  if (!is.null(out$weight)) out$weight <- setYears(out$weight[, "y2025", ], NULL)
  return(out)
}
