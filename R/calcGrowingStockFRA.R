#' @title calcGrowingStockFRA
#' @description
#' Growing stock of one FRA 2025 forest category at country level, all reporting years.
#' A country without a growing stock entry gets the planted-forest growing stock (plantation,
#' other planted forest) or the whole-forest growing stock (naturally regenerating forest).
#' The weight is the category area; a country still without a value gets no weight.
#' Totals of such countries are 0.
#'
#' @param category FRA category: "plantation", "other_planted" or "nat_reg".
#' @param indicator Either "per_ha" (m3/ha, area-weighted) or "total" (Mm3, absolute).
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' calcOutput("GrowingStockFRA", category = "plantation", aggregate = TRUE)
#' }
#'
#' @importFrom magpiesets FRAnames
#' @export

calcGrowingStockFRA <- function(category = "plantation", indicator = "per_ha") {
  if (!category %in% c("plantation", "other_planted", "nat_reg")) {
    stop("Invalid category. Use 'plantation', 'other_planted' or 'nat_reg'.")
  }
  if (!indicator %in% c("per_ha", "total")) {
    stop("Invalid indicator. Use 'per_ha' or 'total'.")
  }
  stock <- readSource("FRA2025", subtype = "growing_stock", convert = TRUE)
  area <- readSource("FRA2025", subtype = "forest_area", convert = TRUE)
  getNames(area) <- FRAnames(getNames(area))

  perHa <- stock[, , grep("gs_ha_", getNames(stock), value = TRUE, fixed = TRUE)]
  getNames(perHa) <- unname(FRAnames(sub("gs_ha_", "", getNames(perHa), fixed = TRUE)))
  cat <- FRAnames(category)
  x <- collapseNames(perHa[, , cat])
  weight <- collapseNames(area[, , cat])
  fallback <- collapseNames(perHa[, , ifelse(category == "nat_reg", "Forest", "Managed Forest")])
  fill <- which(x == 0 & fallback > 0)
  x[fill] <- fallback[fill]
  weight[x == 0] <- 0

  if (indicator == "per_ha") {
    unit <- "m3/ha"
    weight <- weight + 10^-10
  } else {
    total <- stock[, , grep("gs_tot_", getNames(stock), value = TRUE, fixed = TRUE)]
    getNames(total) <- unname(FRAnames(sub("gs_tot_", "", getNames(total), fixed = TRUE)))
    total <- collapseNames(total[, , cat])
    total[fill] <- x[fill] * weight[fill]
    x <- total
    weight <- NULL
    unit <- "Mm3"
  }

  return(list(x = x,
              weight = weight,
              min = 0,
              unit = unit,
              description = paste("Growing stock of", cat, "as reported by FRA 2025,",
                                  "countries without a stock entry filled with planted or whole forest")))
}
