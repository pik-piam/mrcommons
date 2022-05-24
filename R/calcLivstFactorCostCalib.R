#' @title calcLivstFactorCostCalib
#' @description calibrates the global regression for livestock factor requirements to iso level by adjusting the
#' intercept
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("LivstFactorCostCalib")
#' }

calcLivstFactorCostCalib <- function() {

  # elements of factor cost calculation in MAgPIE
  productivity <- calcOutput("LivestockProductivity", aggregate = FALSE)[, 2005, "ssp2", drop = TRUE]
  getNames(productivity) <- c("livst_pig", "livst_rum", "livst_chick", "livst_egg", "livst_milk")
  productionLivst <- collapseDim(calcOutput("Production", products = "kli",
                                            attributes = "dm", aggregate = FALSE))[, 2005, ]
  factorCostsLivst <- calcOutput("FactorCostsLivst", aggregate = FALSE)[, 2005, ]

  # livestock factor cost regression with productivity so far used in MAgpIE:
  categories <- c("livst_chick", "livst_egg", "livst_milk", "livst_pig", "livst_rum", "fish")
  interceptsOld <- c(1500, 1500, 165, 1500, 1042.7, 5258.4)
  slopesOld <- c(0, 0, 38.719, 0, 24355, 0)
  regOld <- new.magpie(cells_and_regions = "GLO", years = NULL, names = categories)
  regOld <- add_dimension(regOld, dim = 3.2, add = NULL, nm = c("cost_regr_a", "cost_regr_b"))
  regOld[, , "cost_regr_a"] <- interceptsOld
  regOld[, , "cost_regr_b"] <- slopesOld

  regOldFish <- regOld[, , "fish", invert = TRUE]

  # calculate new calibration of intercepts, to match USDA/FAO labor costs
  newInterceptsLivst <- (factorCostsLivst / productionLivst) -
                               regOldFish[, , "cost_regr_b", drop = TRUE] * productivity

  regNew <- new.magpie(cells_and_regions = getItems(newInterceptsLivst, dim = 1),
                       years = NULL, names = getItems(regOld, dim = 3))
  for (k in getItems(regOld, dim = 3.1)) {
    regNew[, , list(k, "cost_regr_b")] <- regOld[, , list(k, "cost_regr_b")]
    if (k != "fish") {
      regNew[, , list(k, "cost_regr_a")] <- newInterceptsLivst[, , k]
    } else {
      regNew[, , list(k, "cost_regr_a")] <- regOld[, , list("fish", "cost_regr_a")]
    }
  }

  # fish is not calibrated, so we fill weights with 1 (makes no difference as all regions have the same value)
  prodFish <- new.magpie(cells_and_regions = getItems(productionLivst, dim = 1), years = getYears(productionLivst),
                         names = "fish", fill = 1)
  weight <- mbind(productionLivst, prodFish)

  regNew[!is.finite(regNew)] <- 0
  weight[regNew[, , "cost_regr_a"] == 0] <- 0

  return(list(x = regNew,
              weight = weight,
              unit = "",
              description = "Regression coefficients for livst factor requirements"))
}
