#' @title calcFeedBasketsUncalibrated
#' @description Combines uncalibrated feed baskets of the past with scenario-dependent future feed baskets.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Isabelle Weindl, Benjamin Leon Bodirsky, Stephen Wirth, Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' calcOutput("FeedBasketsUncalibrated")
#' }
#' @importFrom magclass getNames getYears add_columns add_dimension magpie_expand
#' @importFrom utils tail

calcFeedBasketsUncalibrated <- function() {

  past <- findset("past_til2020")
  calibYear <- tail(past, 1)

  outEff <- calcOutput("FeedEfficiencyFuture", aggregate = FALSE)
  getSets(outEff) <- c("iso", "year", "sys", "scen")

  centralFeedShr <- calcOutput("CentralFeedshares", aggregate = FALSE)
  getSets(centralFeedShr) <- c("iso", "year", "sys", "scen")

  fbaskRaw <- calcOutput("FeedBasketsSysPast", aggregate = FALSE)[, past, ]
  getSets(fbaskRaw) <- c("iso", "year", "sys", "kall")
  fbaskSys <- setYears(fbaskRaw[, calibYear, ], NULL)

  # make sure there are no NAs
  fbaskSysTmp <- fbaskSys
  beefDairyMaizPasture <- list(sys = c("sys_beef", "sys_dairy"), kall = c("maiz", "pasture"))
  fbaskSysTmp[, , beefDairyMaizPasture] <- fbaskSys[, , beefDairyMaizPasture] + 10^-10
  pigMaizFoddr <- list(sys = c("sys_pig"), kall = c("maiz", "foddr"))
  fbaskSysTmp[, , pigMaizFoddr] <- fbaskSys[, , pigMaizFoddr] + 10^-10

  kli <- findset("kli")

  composeIdealFeed <- function(main, anti, const, sys) {
    if (anyDuplicated(c(main, const, anti)) != 0) {
      stop("duplicates in main, anti and const")
    }
    if (!all(c(main, const, anti) %in% getNames(fbaskSysTmp, dim = "kall"))) {
      stop("not all feed items assigned to main,const,anti")
    }

    # estimate the composition of the sub-baskets
    compositionMain <- fbaskSysTmp[, , main][, , sys] / dimSums(fbaskSysTmp[, , main][, , sys], dim = "kall")
    compositionAnti <- fbaskSysTmp[, , anti][, , sys] / dimSums(fbaskSysTmp[, , anti][, , sys], dim = "kall")

    constant <- fbaskSysTmp[, , const][, , sys]
    constantSum <- dimSums(constant[, , sys], dim = "kall")

    # compose the value chain
    mainBask <- outEff[, , sys] * centralFeedShr[, , sys]
    antiBask <- (outEff[, , sys] * (1 - centralFeedShr[, , sys])) - constantSum

    reduceConstant <- antiBask
    reduceConstant[reduceConstant > 0] <- 0
    reductionFactorConstant <- toolConditionalReplace((constantSum + reduceConstant) / constantSum, "is.na()", 1)

    antiBask[antiBask < 0] <- 0
    mainBask <- mainBask * compositionMain
    antiBask <- antiBask * compositionAnti

    # the first term just extends the time dimension
    constant <- (centralFeedShr[, , sys] * 0 + 1) * constant * reductionFactorConstant

    bask <- mbind(mainBask, antiBask, constant)
    return(bask)
  }

  ### Ruminants
  mainRum <- c("res_cereals", "res_fibrous", "res_nonfibrous", "pasture")
  constRum <- c("brans", findset("kap"), "potato", "puls_pro", "sugr_beet", "sugr_cane", "groundnut")
  baskRum <- composeIdealFeed(
    main = mainRum,
    anti = setdiff(getNames(fbaskSys, dim = "kall"), c(mainRum, constRum)),
    const = constRum,
    sys = c("sys_beef", "sys_dairy")
  )

  ### Pigs
  constPig <-  c(findset("kap"), "potato", "puls_pro", "sugr_beet", "sugr_cane", "groundnut")
  antiPig    <- c("tece", "trce", "maiz", "rice_pro",
                       "others", "potato", "cassav_sp", "puls_pro",
                       "soybean", "rapeseed", "groundnut", "sunflower", "oilpalm", "cottn_pro",
                       "sugr_beet", "sugr_cane",
                       "livst_rum", "livst_pig", "livst_chick", "livst_egg", "livst_milk", "fish")
  antiPig <- setdiff(antiPig, constPig)
  baskPig <- composeIdealFeed(
    main = setdiff(getNames(fbaskSys, dim = "kall"), c(antiPig, constPig)),
    anti = antiPig,
    const =  constPig,
    sys = c("sys_pig")
  )

  ### Chicken
  sys <- c("sys_chicken", "sys_hen")
  baskChick <- outEff[, , sys] * fbaskSys[, , sys] / dimSums(fbaskSys[, , sys], dim = "kall")

  out <- mbind(
    baskRum,
    baskPig,
    baskChick
  )

  out <- round(out, 3)

  if (any(out[, , "sys_dairy"][, , "livst_milk"] >= 1)) {
    stop("more livestock products in feed basket than being produced")
  }
  if (any(out[, , "sys_hen"][, , "livst_egg"] >= 1)) {
    stop("more livestock products in feed basket than being produced")
  }


  # use livestock production as weight
  kli <- findset("kli")
  faoMassBalance <- calcOutput("FAOmassbalance_pre", aggregate = FALSE)
  weightKli <- collapseNames(faoMassBalance[, , kli][, , "dm"][, , "production"][, past, ])
  prodSysRatio <- calcOutput("ProdSysRatioPast", aggregate = FALSE)[, past, ]

  weightSys <- dimSums(weightKli * prodSysRatio, dim = "ItemCodeItem")
  weightSys <- toolHoldConstantBeyondEnd(weightSys)

  return(list(x = out,
              weight = weightSys,
              unit = "tDM / t DM",
              description = "Uncalibrated feed requirements in DM per DM products generated for 5 livestock systems"))

}
