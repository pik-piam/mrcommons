#' @title calcFAOmassbalance
#' @description
#' Calculates a massbalance dataset of agricultural production, processing and
#' use out of the combined data of calcFAOharmonized(). Covers dry matter (DM),
#' reactive nitrogen (Nr), Phosphorus (P), Generalizable Energy (GE) and wet
#' matter (WM). New products are added to the Food Balance Sheets, and many processing
#' conversions are made more explicit using simple assumptions. The first part of this
#' function is the calcFAOmassbalance_pre.
#'
#' @param version whether to use the pre ("pre2010") or post ("post2010") 2010 versions of FAOSTAT Food balances,
#' or "join2010" which joins them at 2010
#' @param yearly whether to calculate yearly data or only magpie 5year timesteps
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Xiaoxi Wang, David Chen
#' @seealso
#' [mrfaocore::calcFAOmassbalance_pre()]
#' @examples
#' \dontrun{
#' calcOutput("FAOmassbalance")
#' }

#' @importFrom magclass getNames<- as.magpie
#' @importFrom withr local_options


calcFAOmassbalance <- function(version = "join2010", yearly = FALSE) {
  local_options(magclass_sizeLimit = 1e+12)

  if (version == "join2010") {
    past <- findset("past_til2020")
  } else if (version == "pre2010") {
    past <- findset("past")
  } else if (version == "post2010") {
    past <- c("y2010", "y2015", "y2020")
  }

  mb <- calcOutput("FAOmassbalance_pre", version = version, aggregate = FALSE)
  if (yearly == FALSE) {
    mb <- mb[, past, ]
  }
  mb1 <- add_columns(mb, dim = 3.2, addnm = "bioenergy")
  mb1[, , "bioenergy"] <- 0
  items <- intersect(getItems(mb1, dim = 3.2), c("production", "production_estimated",
                                                 "export", "import", "stock_variation",
                                                 "domestic_supply",
                                                 "food", "feed", "seed", "waste", "other_util", "bioenergy",
                                                 "milling", "brans1", "branoil1", "flour1",
                                                 "refining", "sugar1", "sugar2", "sugar3", "molasses1",
                                                 "extracting", "oil1", "oil2", "oilcakes1",
                                                 "fermentation", "alcohol1",
                                                 "alcohol2", "alcohol3", "alcohol4", "brewers_grain1",
                                                 "distilling", "ethanol1", "distillers_grain1",
                                                 "distillingloss",
                                                 "households"))
  mb1 <- mb1[, , items]
  newitems <- setdiff(findset("kall"), getNames(mb1, dim = 1))
  mb2 <- add_columns(mb1, dim = 3.1, addnm = newitems)
  mb2[, , newitems] <- 0

  ### Add feed by animal group
  # consists of feed by animal group according to Isabelle Weindls Feed Baskets
  # plus a balanceflow to be consistent with FAO
  feed <- calcOutput("FeedPast", balanceflow = FALSE, aggregate = FALSE, yearly = yearly)
  getNames(feed, dim = 1) <- paste0("feed_", substring(getNames(feed, dim = 1), 7))
  feed <- as.magpie(aperm(unwrap(feed), c(1, 2, 4, 3, 5)))

  balanceflow <- calcOutput("FeedBalanceflow", aggregate = FALSE, future = FALSE, yearly = yearly)
  getNames(balanceflow, dim = 1) <- paste0("feed_", getNames(balanceflow, dim = 1))
  balanceflow <- balanceflow * calcOutput("Attributes", aggregate = FALSE)
  balanceflow <- as.magpie(aperm(unwrap(balanceflow), c(1, 2, 4, 3, 5)))

  feed <- feed + balanceflow
  # test to check whether distribution of feed was ok
  cyears <- intersect(getYears(mb2), getYears(feed))
  if (any(round(
                mb2[, cyears, "feed"][, , getNames(mb, dim = 1)]
                - dimSums(feed[, cyears, getNames(mb, dim = 1)], dim = 3.2),
                7) != 0)) {
    vcat(verbosity = 1, "Something is strange here. Check Feedbalanceflow")
  }
  mb3 <- mbind(mb2[, cyears, ], feed[, cyears, ])

  forest <- calcOutput("TimberDemand", aggregate = FALSE)
  # quick fix, make 2020 same as 2019
  forest2020 <- forest[, 2019, ]
  forest2020 <- setYears(forest2020, 2020)
  forest <- mbind(forest, forest2020)
  # Convert m3 to tDM using IPCC climate-region wood density (tDM per m3)
  woodDensity <- calcOutput("WoodDensity", aggregate = FALSE)
  # FAO woodfuel stacking correction: FAO woodfuel statistics are widely reported
  # in stacked m3 (stere) rather than solid m3, overstating volumes by ~35%.
  # Standard stacking factor: 1 stere = 0.65 solid m3.
  # Sources: FAO (2004) UWET Section 5.1.3; FAO/ITTO/UNECE (2020) Table 2.2.
  forest[, , "Wood fuel"] <- forest[, , "Wood fuel"] * 0.65
  mb3[, , getNames(mb3[, , paste0("wood.",
                                  getNames(forest, dim = 2),
                                  ".dm")])] <- forest[, intersect(getYears(mb3),
                                                                  getYears(forest)),
                                                      getNames(forest[, , "Industrial roundwood"])] * woodDensity
  mb3[, , getNames(mb3[, , paste0("woodfuel.",
                                  getNames(forest, dim = 2),
                                  ".dm")])] <- forest[, intersect(getYears(mb3),
                                                                  getYears(forest)),
                                                      getNames(forest[, , "Wood fuel"])] * woodDensity

  # Adding Pasture as feed item
  mb3[, , "pasture"][, ,
                     c("domestic_supply", "production", "feed")] <- dimSums(feed[, getYears(mb3), "pasture"], dim = 3.2)

  # Adding Crop Residues Production and use
  kres <- findset("kres")
  res <- calcOutput("ResDemand", aggregate = FALSE, yearly = yearly)

  res <- as.magpie(aperm(unwrap(res), c(1, 2, 4, 3, 5)))
  mb3[, , kres][, , c("bioenergy", "domestic_supply", "feed", "other_util", "production")] <- res[, getYears(mb3), ]

  ##############################################################################
  ### Dividing other_util into bioenergy and other_util
  bioenergy <- calcOutput("1stBioenergyPast", aggregate = FALSE)
  att <- calcOutput("Attributes", aggregate = FALSE)
  ethanolOilFactor <- (att / (collapseNames(att[, , "ge"])))[, , c("ethanol", "oils")]
  bioenergy <- bioenergy[, getYears(mb3), c("ethanol", "oils")] * ethanolOilFactor

  # in IEA, TES (total energy supply) = INDPROD (indigenous production) + IMPORTS + EXPORTS (with negative values).
  # for us, TES is bioenergy use.
  mb3[, , c("ethanol", "oils")
  ][, , "bioenergy"] <- dimSums(bioenergy[, , c("ethanol", "oils")
                                ][, , c("INDPROD", "IMPORTS", "EXPORTS")] # exports are negative
                                , dim = 3.2)

  # For ethanol where we dont have trade data, we use the trade data from IEA.
  mb3[, , c("ethanol")][, , "import"] <- (bioenergy[, , "ethanol"])[, , "IMPORTS", drop = TRUE]
  mb3[, , c("ethanol")][, , "export"] <- -(bioenergy[, , "ethanol"]
  )[, , "EXPORTS", drop = TRUE] # exports are negative
  mb3[, , c("ethanol")][, , "other_util"]  <- mb3[, , "production"][, , "ethanol"] +
    mb3[, , "import"][, , "ethanol"] -
    mb3[, , "export"][, , "ethanol"]

  # in some cases bioenergy demand from EEA exceeds ethanol production.
  # We limit it to the availabiltiy in FAOmassbalance_pre
  exceeded <- ifelse(collapseNames(mb3[, , c("ethanol", "oils")][, , "bioenergy"]) >
                       collapseNames(mb3[, , c("ethanol", "oils")][, , "other_util"]),
                     collapseNames(mb3[, , c("ethanol", "oils")][, , "bioenergy"]) -
                       collapseNames(mb3[, , c("ethanol", "oils")][, , "other_util"]),
                     0)

  mb3[, , c("ethanol", "oils")][, , "bioenergy"] <- mb3[, , c("ethanol", "oils")][, , "bioenergy"] - exceeded

  mb3[, , c("ethanol", "oils")][, , "other_util"] <- mb3[, , c("ethanol", "oils")][, , "other_util"] -
    mb3[, , c("ethanol", "oils")][, , "bioenergy"]



  ### round to 1 ton to avoid calculation issues
  mb3 <- round(mb3, 6)

  return(list(
              x = mb3,
              weight = NULL,
              unit = "Mt DM, Mt WM, PJ, Mt Nr, Mt P, Mt K",
              description = "FAO massbalance calculates all conversion processes
              within the FAO CBS/FBS and makes them explict."))
}
