#' @title calcFeedBalanceflow
#' @description Calculates feed balanceflows from MAgPIE-Feed model to meet FAO data
#'
#' @param per_livestock_unit default false
#' @param cellular   if TRUE value is calculate on cellular level
#' @param products products in feed baskets that shall be reported
#' @param future if FALSE, only past years will be reported (reduces memory)
#' @return List of magpie objects with results on country or cellular level, unit and description.
#' @author Isabelle Weindl, Kristine Karstens
#' @examples
#' \dontrun{
#' calcOutput("FeedBalanceflow")
#' }
#'
#' @importFrom magclass getNames lowpass convergence

calcFeedBalanceflow <- function(per_livestock_unit = FALSE, # nolint
                                cellular = FALSE,
                                products = "kall",
                                future = "constant") {

  perLivestockUnit <- per_livestock_unit # nolint

  products2 <- findset(products, noset = "orignal")
  past      <- findset("past_til2020")

  if (!perLivestockUnit) {
    prodAttributes      <- calcOutput("Attributes", aggregate = FALSE)

    faoFeednutrients <- collapseNames(calcOutput("FAOmassbalance_pre", aggregate = FALSE)[, past, "feed"])
    faoFeed          <- collapseNames(faoFeednutrients[, , "dm"])
    faoFeed          <- add_columns(faoFeed, addnm = "pasture", dim = 3.1)

    magFeednutrients <- calcOutput("FeedPast", balanceflow = FALSE, cellular = FALSE,
                                   aggregate = FALSE, nutrients = "all", products = products)
    magFeed          <- magFeednutrients[, , "dm"]

    magFeedShare     <- magFeed / dimSums(magFeed, dim = 3.1)
    magFeedShare[is.nan(magFeedShare)] <- 0
    commonproducts <- intersect(getNames(faoFeed, dim = 1), getNames(magFeed, dim = 2))

    # include estimates for pasture feed demand to benchmark data faoFeed:
    faoFeed[, , "pasture"] <- collapseNames(dimSums(magFeed, dim = 3.1))[, , "pasture"]
    # reduced pasture feed demand (which is determined in the feed model as balance post)
    # by the amount of fish that is used as feed and not yet considered in feed baskets
    # replacement is done on the basis of proteins and cannot exceed 50% of the pasture feed demand:
    reducedgraz <- (collapseNames(dimSums(magFeednutrients[, , "pasture"], dim = 3.1))[, , "nr"]
                    - collapseNames(faoFeednutrients[, , "fish"])[, , "nr"]) /
      prodAttributes[, , "nr.pasture"]
    faoFeed[, , "pasture"][which(reducedgraz > 0.5 * faoFeed[, , "pasture"])] <-
      reducedgraz[which(reducedgraz > 0.5 * faoFeed[, , "pasture"])]
    faoFeed[, , "pasture"][which(reducedgraz < 0.5 * faoFeed[, , "pasture"])] <-
      0.5 * faoFeed[, , "pasture"][which(reducedgraz < 0.5 * faoFeed[, , "pasture"])]

    ## adjusted feed shares of pasture and 'indefinite' feed ressources for ruminants in South and Central Asia:
    # Table 3.28, Wirsenius 2000
    rumPastshrInd <- 0.360  # Permanent pasture (including browse)
    rumScavshrInd <- 0.225  # Herbage and browse from forest and other land & thinning and weeding in cropland
    faoFeed["IND", , "pasture"] <- (rumPastshrInd / (rumScavshrInd + rumPastshrInd)) * faoFeed["IND", , "pasture"]

    # reduce temporal variability of estimated pasture feed demand:
    faoFeed[, , "pasture"] <- lowpass(faoFeed[, , "pasture"], i = 3)

    # calculate feed balance flows:
    feedBalanceflow  <- faoFeed[, , commonproducts] - dimSums(magFeed, dim = 3.1)[, , commonproducts]
    feedBalanceflow2 <- collapseNames(magFeedShare[, , commonproducts] * feedBalanceflow)

    feedBalanceflow2[is.nan(feedBalanceflow2)] <- 0
    feedBalanceflow2[is.na(feedBalanceflow2)]  <- 0

    if (any(round(dimSums(feedBalanceflow2, dim = 3.1) - feedBalanceflow, 5) != 0)) {

      vcat(verbosity = 2, paste(
        "Difficult to distribute the balanceflow between different livestock",
        "commodities, because it is not used at all in the feedbaskets.",
        "Distributed to ruminants for now."))
      overflow                                <- feedBalanceflow - dimSums(feedBalanceflow2, dim = 3.1)
      feedBalanceflow2[, , "alias_livst_rum"] <- feedBalanceflow2[, , "alias_livst_rum"] + overflow

    }

    feedBalanceflow  <- feedBalanceflow2

    if (cellular) {

      countryToCell <- toolGetMappingCoord2Country()
      countryToCell$coordiso <- paste(countryToCell$coords, countryToCell$iso, sep = ".")
      magFeedCell      <- calcOutput("FeedPast", balanceflow = FALSE,
                                     cellular = TRUE,
                                     aggregate = FALSE, nutrients = "dm", products = products)
      magFeedCell      <- magFeedCell[, , commonproducts]
      magFeedCountry   <- toolAggregate(magFeedCell, rel = countryToCell,
                                        from = "coordiso", to = "iso", dim = 1, partrel = TRUE)
      magFeedCellshare <- collapseNames(magFeedCell / magFeedCountry)
      magFeedCellshare[is.na(magFeedCellshare)] <- 0

      feedBalanceflow  <- toolAggregate(feedBalanceflow, rel = countryToCell, from = "iso",
                                        to = "coordiso", dim = 1, partrel = TRUE)

      for (livst_x in getNames(feedBalanceflow, dim = 1)) {
        feedBalanceflow[, , livst_x]  <- feedBalanceflow[, , livst_x] * magFeedCellshare[, , livst_x]
      }
    }

    # add items that are not present in the FAO massbalance pre
    newItems         <- setdiff(products2, getNames(faoFeed, dim = 1))
    feedBalanceflow  <- add_columns(feedBalanceflow, addnm = newItems, dim = 3.2)
    feedBalanceflow[, , newItems] <- 0

    if (future == TRUE) {
      feedBalanceflow  <- toolHoldConstantBeyondEnd(feedBalanceflow)
      # fading out the balanceflow until 2050.
      # Has to be the same as the SlaugherBalanceflow outfade!
      feedBalanceflow  <- convergence(origin = feedBalanceflow, aim = 0,
                                      start_year = "y2010", end_year = "y2050", type = "s")
    } else if (future == "constant") {
      feedBalanceflow  <- toolHoldConstantBeyondEnd(feedBalanceflow)
      # Has to be the same as the SlaugherBalanceflow outfade!
    }

    weight <- NULL
    unit   <- "t DM"
    getNames(feedBalanceflow, dim = 1) <- substring(getNames(feedBalanceflow, dim = 1), 7)

  } else if (perLivestockUnit) {

    kli  <- findset("kli")
    past <- findset("past")

    feedBalanceflow <- calcOutput("FeedBalanceflow", cellular = cellular,
                                  products = products, future = future, aggregate = FALSE)
    livestockProduction <- collapseNames(calcOutput("Production", products = "kli",
                                                    cellular = cellular,
                                                    aggregate = FALSE)[, , kli][, past, "dm"])
    livestockProduction <- add_columns(livestockProduction, addnm = "fish", dim = 3.1)
    livestockProduction[, , "fish"] <- 0

    if (!cellular) {
      livestockProduction <- toolHoldConstantBeyondEnd(livestockProduction)
    }

    feedBalanceflow <- feedBalanceflow / livestockProduction
    feedBalanceflow[is.na(feedBalanceflow)] <- 0
    feedBalanceflow[is.infinite(feedBalanceflow)] <- 0

    weight <- livestockProduction
    unit   <- "1"

  } else {
    stop("per_livestock_unit has to be boolean")
  }

  return(list(x = feedBalanceflow,
              weight = weight,
              unit = unit,
              description = "Difference between feed baskets and feed use by FAO",
              isocountries = !cellular)
  )
}
