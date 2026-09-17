#' @title calcSeed
#' @description Calculates Seed demand
#'
#' @param cellular   cellular or regional level
#' @param cells      Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param products   kcr or also kall, which includes seeds for eggs and fish
#' @param irrigation if TRUE, distinguishes irrigated and non-irrigated crops
#' @param attributes in dm, wm, ge, nr, p, k
#' @return List of magpie object with results and weight on country or cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("Seed")
#' }
#' @importFrom magpiesets findset

calcSeed <- function(cellular = FALSE, cells = "lpjcell", products = "kall",
                     irrigation = FALSE, attributes = "all") {

  products <- findset(products, noset = "original")
  seed     <- collapseNames(calcOutput("FAOmassbalance", aggregate = FALSE)[, , "seed"][, , products])

  if (cellular) {
    if (!all(products %in% findset("kcr"))) {
      stop("cellular data only exists for kcr products")
    }
    productionReg <- calcOutput("Production", products = "kcr", cellular = FALSE, calibrated = TRUE,
                                 irrigation = FALSE, aggregate = FALSE)[, getItems(seed, dim = 2), products]
    seedShr       <- collapseNames(seed[, , "dm"] / productionReg[, , "dm"])
    seedShr[is.na(seedShr)]       <- 0
    seedShr[is.infinite(seedShr)] <- 0

    production     <- calcOutput("Production", products = "kcr",
                                 cellular = cellular,
                                 irrigation = irrigation,
                                 calibrated = TRUE, attributes = attributes, aggregate = FALSE)[, , products]
    seed           <- production * seedShr[getItems(production, dim = 1.3), , ]

    if (cells == "magpiecell") {
      seed <- toolCoord2Isocell(seed, cells = cells)
    }
  }

  if (any(attributes != "all")) {
    seed <- seed[, , attributes]
  }

  return(list(
    x = seed,
    weight = NULL,
    unit = "Mt Dm, Nr, P, K, WM or PJ Energy",
    description = "Seed use",
    isocountries = !cellular))
}
