#' @title calcFeedPast
#' @description Combines feed baskets of the past with livestock production to get total feed demand
#'
#' @param balanceflow if TRUE, non-eaten food is included in feed baskets, if not it is excluded.
#' @param products    products in feed baskets that shall be reported
#' @param cellular    if TRUE value is calculate on cellular level with returned datajust in dry matter
#' @param cells       Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param nutrients   nutrients like dry matter (DM), reactive nitrogen (Nr), Phosphorus (P),
#'                    Generalizable Energy (GE) and wet matter (WM).
#' @return List of magpie objects with results on country or cellular level, unit and description.
#' @author Isabelle Weindl, Benjamin Leon Bodirsky, Kristine Karstems
#' @examples
#' \dontrun{
#' calcOutput("FeedPast")
#' }
#' @importFrom magpiesets findset
#' @importFrom magclass getNames

calcFeedPast <- function(balanceflow = TRUE, cellular = FALSE, cells = "lpjcell",
                         products = "kall", nutrients = "all") {

  if (cellular && (length(nutrients) > 1)) {
    stop("out of memory reasons, cellular datasets can only be used with one nutrient")
  }
  if (cellular && (products == "kall")) {
    cat("because of memory reasons, cellular datasets can often not be run with kall yet; try kfeed")
  }

  past <- findset("past_til2020")
  products2           <- findset(products, noset = "original")
  
  kliProduction       <- calcOutput("Production", products = "kli", 
                                    cellular = cellular, cells = "lpjcell", aggregate = FALSE)
  livestockProduction <- collapseNames(kliProduction[, past, "dm"])
  animalProduction    <- add_columns(livestockProduction, addnm = "fish", dim = 3.1)
  animalProduction[, , "fish"]        <- 0
  getNames(animalProduction, dim = 1) <- paste0("alias_", getNames(animalProduction, dim = 1))

  feedBaskets         <- calcOutput("FeedBasketsPast", non_eaten_food = FALSE, aggregate = FALSE)
  feedBaskets         <- feedBaskets[, , products2]

  #extend feedBaskets to 2020 constantly for now
  feedBaskets <- toolHoldConstant(feedBaskets, years = c(2015, 2020))

  if (cellular) {
    feedBaskets <- toolIso2CellCountries(feedBaskets, cells = "lpjcell")
  }

  feedConsumption  <- animalProduction * feedBaskets
  min              <- 0

  if (balanceflow) {
    balanceflow <- calcOutput("FeedBalanceflow", cellular = cellular, products = products, aggregate = FALSE)
    getNames(balanceflow, dim = 1) <- paste0("alias_", getNames(balanceflow, dim = 1))
    feedConsumption <- feedConsumption + balanceflow[getCells(feedConsumption), getYears(feedConsumption),
                                                     getNames(feedConsumption)]
    min <- -Inf

  } else if (balanceflow != FALSE) {
    stop("balanceflow has to be boolean")
  }

  feedConsumption     <- round(feedConsumption, 8)

  prodAttributes      <- calcOutput("Attributes", aggregate = FALSE)
  if (all(nutrients != "all")) {
    prodAttributes <- prodAttributes[, , nutrients]
  }
  feedConsumption     <- feedConsumption * prodAttributes[, , products2]
  unit                <- "Mt DM/Nr/P/K/WM or PJ energy"
  description         <- paste("Feed: dry matter: Mt (dm), gross energy: PJ (ge), reactive nitrogen: Mt (nr),",
                               "phosphor: Mt (p), potash: Mt (k), wet matter: Mt (wm).")

  if (cellular) {
    if (cells == "magpiecell") {
      feedConsumption <- toolCoord2Isocell(feedConsumption, cells = cells)
    }
  }

  return(list(x = feedConsumption,
              weight = NULL,
              unit = unit,
              min = min,
              description = description,
              isocountries = !cellular))
}
