#' @title calcFeedBalanceflow
#' @description Calculates feed balance flows from MAgPIE-Feed model to meet FAO data
#'
#' @param per_livestock_unit default false
#' @param cellular   if TRUE value is calculated on cellular level
#' @param products products in feed baskets that shall be reported
#' @param future if FALSE, only past years will be reported (reduces memory)
#' @param yearly whether to calculate yearly data or only magpie 5year timesteps
#' @return List of magpie objects with results on country or cellular level, unit and description.
#'
#' @author Isabelle Weindl, Kristine Karstens
#' @examples
#' \dontrun{
#' calcOutput("FeedBalanceflow")
#' }
calcFeedBalanceflow <- function(per_livestock_unit = FALSE, # nolint
                                cellular = FALSE,
                                products = "kall",
                                future = "constant",
                                yearly = FALSE) {

  perLivestockUnit <- per_livestock_unit # nolint

  products2 <- findset(products, noset = "orignal")
  past      <- findset("past_til2020")

  if (!perLivestockUnit) {
    prodAttributes      <- calcOutput("Attributes", aggregate = FALSE)

    faoFeednutrients <- collapseNames(calcOutput("FAOmassbalance_pre", aggregate = FALSE)[, , "feed"])
    if (yearly == FALSE) {
      cyears <- intersect(past, getYears(faoFeednutrients))
      faoFeednutrients <- faoFeednutrients[, cyears, ]
    }
    faoFeed          <- collapseNames(faoFeednutrients[, , "dm"])
    faoFeed          <- add_columns(faoFeed, addnm = "pasture", dim = 3.1)

    magFeednutrients <- calcOutput("FeedPast", balanceflow = FALSE, cellular = FALSE,
                                   aggregate = FALSE, nutrients = "all", products = products, yearly = yearly)
    magFeed          <- magFeednutrients[, , "dm"]

    magFeedShare     <- magFeed / dimSums(magFeed, dim = 3.1)
    magFeedShare[is.nan(magFeedShare)] <- 0
    commonproducts <- intersect(getNames(faoFeed, dim = 1), getNames(magFeed, dim = 2))
    faoFeed <- faoFeed[, getYears(magFeed), ]
    faoFeednutrients <- faoFeednutrients[, getYears(magFeed), ]
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

    ## adjusted feed shares of pasture and 'indefinite' feed resources for ruminants in South and Central Asia:
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
                                     aggregate = FALSE, nutrients = "dm",
                                     products = products, yearly = yearly)
      magFeedCell      <- magFeedCell[, , commonproducts]
      magFeedCountry   <- toolAggregate(magFeedCell, rel = countryToCell,
                                        from = "coordiso", to = "iso", dim = 1, partrel = TRUE)
      magFeedCellshare <- collapseNames(magFeedCell / magFeedCountry)
      magFeedCellshare[is.na(magFeedCellshare)] <- 0

      feedBalanceflow  <- toolAggregate(feedBalanceflow, rel = countryToCell, from = "iso",
                                        to = "coordiso", dim = 1, partrel = TRUE)
      cyears           <- intersect(getYears(feedBalanceflow), getYears(magFeedCellshare))

      for (livst_x in getNames(feedBalanceflow, dim = 1)) {
        feedBalanceflow[, cyears, livst_x]  <- feedBalanceflow[, cyears, livst_x] * magFeedCellshare[, cyears, livst_x]
      }
    }

    # add items that are not present in the FAO massbalance pre
    newItems         <- setdiff(products2, getNames(faoFeed, dim = 1))
    feedBalanceflow  <- add_columns(feedBalanceflow, addnm = newItems, dim = 3.2)
    feedBalanceflow[, , newItems] <- 0

    if (isTRUE(future)) {
      feedBalanceflow  <- toolHoldConstantBeyondEnd(feedBalanceflow)
      # fading out the balanceflow until 2050.
      # Has to be the same as the SlaugherBalanceflow outfade!
      pastYears        <- getYears(feedBalanceflow)
      feedBalanceflow  <- convergence(origin = feedBalanceflow, aim = 0,
                                      start_year = pastYears[length(pastYears)], end_year = "y2050", type = "s")
    } else if (future == "constant") {
      feedBalanceflow  <- toolHoldConstantBeyondEnd(feedBalanceflow)
      # Has to be the same as the SlaugherBalanceflow outfade!
    }

    weight <- NULL
    unit   <- "t DM"
    getNames(feedBalanceflow, dim = 1) <- substring(getNames(feedBalanceflow, dim = 1), 7)

  } else if (perLivestockUnit) {

    kli  <- findset("kli")
    past <- findset("past_til2020")

    feedBalanceflow <- calcOutput("FeedBalanceflow", cellular = cellular,
                                  products = products, future = future, aggregate = FALSE,
                                  yearly = yearly)
    livestockProduction <- collapseNames(calcOutput("Production", products = "kli",
                                                    cellular = cellular,
                                                    aggregate = FALSE)[, , kli][, , "dm"])
    if (yearly == FALSE) {
      cyears <- intersect(past, getYears(livestockProduction))
      livestockProduction <- livestockProduction[, cyears, ]
    }
    livestockProduction <- add_columns(livestockProduction, addnm = "fish", dim = 3.1)
    livestockProduction[, , "fish"] <- 0

    if (!cellular) {
      livestockProduction <- toolHoldConstantBeyondEnd(livestockProduction)
    }

    cyears <- intersect(getYears(feedBalanceflow), getYears(livestockProduction))
    feedBalanceflow <- feedBalanceflow[, cyears, ] / livestockProduction[, cyears, ]
    feedBalanceflow[is.na(feedBalanceflow)] <- 0
    feedBalanceflow[is.infinite(feedBalanceflow)] <- 0

    weight <- livestockProduction[, cyears, ]
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
