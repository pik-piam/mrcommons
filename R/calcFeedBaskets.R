#' @title calcFeedBaskets
#' @description Combines feed baskets of the past
#'              with scenario-dependent future feed baskets.
#'
#' @param non_eaten_food if TRUE, non-eaten food is included in feed baskets,
#'                       if not it is excluded.
#' @param fadeout        if TRUE, feed basket calibration fades out till 2050.
#' @param method         "new" for additive calibration at end,
#'                       "old" for multiplicative calibration of calShr and end values.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Isabelle Weindl, Benjamin Leon Bodirsky, Stephen Wirth, Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' calcOutput("FeedBaskets")
#' }
#' @importFrom magclass mselect getNames getYears add_columns add_dimension magpie_expand convergence
#' @importFrom luscale rename_dimnames
#' @importFrom utils tail

calcFeedBaskets <- function(non_eaten_food = FALSE, # nolint
                            fadeout = FALSE, method = "new") {

  if (method == "new") {
    feedBasketsUNCALIB <- calcOutput("FeedBasketsUncalibrated", aggregate = FALSE)
    fbaskSys           <- calcOutput("FeedBasketsSysPast", aggregate = FALSE)
    getSets(fbaskSys)  <- c("iso", "year", "sys", "kall")

    fbaskSys <- toolHoldConstantBeyondEnd(fbaskSys)

    past  <- findset("past_til2020")
    calib <- fbaskSys[, past, ] - feedBasketsUNCALIB[, past, ]
    calibCONST       <- toolHoldConstantBeyondEnd(calib)
    calibDECLINE2050 <- convergence(origin = calibCONST, aim = 0,
                                    start_year = 2020, end_year = 2050, type = "s")

    if (fadeout) {
      calibChosen <- calibDECLINE2050
    } else {
      calibChosen <- calibCONST
    }

    out <- calibChosen + feedBasketsUNCALIB

    # do not allow the calibration to introduce the inconsistency of
    # more livestock products in feed basket than produced;
    # apply a reduction factor for calibration to avoid this
    milkInpDairy <- list(sys = "sys_dairy", kall = "livst_milk") # item showing milk input for dairy production
    if (any(feedBasketsUNCALIB[, , milkInpDairy] >= 1)) {
      stop("more livestock products in feed basket than being produced already for uncalibrated feed baskets")
    }
    calibReductionFact <- (1 - feedBasketsUNCALIB[, , milkInpDairy]) /
      calibChosen[, , milkInpDairy] - .Machine$double.eps
    calibReductionFact[(calibReductionFact >= 1) | (calibReductionFact <= 0)] <- 1
    # Note that calibReductionFact can only be different from 1 (i.e. between 0 and 1), if the positive difference
    # "1 - feedBasketsUNCALIB[,,milkInpDairy]" is smaller than the then positive "calibChosen[,,milkInpDairy]";
    # so adding "calibChosen" to "feedBasketsUNCALIB" would cause the inconsistency of out[, , milkInpDairy] > 1.
    # Then the reduction gives "out[, , milkInpDairy] = 1" so its the smallest possible change giving consistency.
    out[, , milkInpDairy] <-
      calibChosen[, , milkInpDairy] * calibReductionFact + feedBasketsUNCALIB[, , milkInpDairy]

    out[out < 0] <- 0

    # change from sys to kli
    prodSysRatio <- calcOutput("ProdSysRatioPast", aggregate = FALSE)
    prodSysRatio <- toolHoldConstantBeyondEnd(prodSysRatio)
    out <- dimSums(out * prodSysRatio, dim = "sys")

    ### careful from here on: livst_milk is in two of the sets of out
    if (non_eaten_food == FALSE) {
      otherFoods <- setdiff(getNames(out, dim = "kall"), "non_eaten_food")
      out        <- out[, , list(kall = otherFoods)]
    }
    out <- dimOrder(out, perm = c(3, 1, 2))

    weightKLI <- collapseNames(calcOutput("FAOmassbalance_pre",
                                          aggregate = FALSE)[, past, findset("kli")][, , "dm"][, , "production"])
    weightKLI <- toolHoldConstantBeyondEnd(weightKLI)

    if (any(out[, , list(data1 = "livst_milk", kall = "livst_milk")] >= 1)) {
      stop("more livestock products in feed basket than being produced")
    }

    return(list(x = out,
                weight = weightKLI,
                unit = "tDM per tDM",
                min = 0,
                description = "Detailed feed requirements in DM per DM products generated for 5 livestock commodities"))

  } else {

    fbaskSys <- calcOutput("FeedBasketsSysPast", aggregate = FALSE)

    # create MAgPIE objects which contain 1 for entries belonging to the main share
    # and 0 for entries belonging to the anti share
    .belong2type <- function(commodities, elems) {
      mainshrRum <- c("res_cereals", "res_fibrous", "res_nonfibrous", "pasture")
      antishrPig <- c("tece", "trce", "maiz", "rice_pro",
                      "others", "potato", "cassav_sp", "puls_pro",
                      "soybean", "rapeseed", "groundnut", "sunflower", "oilpalm", "cottn_pro",
                      "sugr_beet", "sugr_cane",
                      "livst_rum", "livst_pig", "livst_chick", "livst_egg", "livst_milk", "fish")
      mainshrPoultry <- c("distillers_grain", "molasses", "oilcakes", "brans")

      mainshrPig <- commodities[-match(antishrPig, commodities, nomatch = FALSE)]

      main <- new.magpie(cells_and_regions = "GLO",
                         years = NULL,
                         names = elems,
                         fill = 0)

      const <- add_dimension(main, dim = 3.3, add = "type", nm = "const")

      main[, , mainshrRum][, , c("sys_dairy", "sys_beef")] <- 1
      main[, , mainshrPig][, , "sys_pig"] <- 1
      main[, , mainshrPoultry][, , c("sys_chicken", "sys_hen")] <- 1

      anti <- add_dimension(1 - main, dim = 3.3, add = "type", nm = "anti")
      main <- add_dimension(main, dim = 3.3, add = "type", nm = "main")

      # define feed commodities that do not change over time; they should not belong to the feed commodities
      # that define the main feed share used for the regression analysis
      constshr <- c(findset("kap"), "potato", "puls_pro", "sugr_beet", "sugr_cane", "groundnut")
      mselect(const, data1 = constshr) <- 1
      mselect(anti, data1 = constshr)  <- 0
      mselect(main, data1 = constshr)  <- 0
      # additionally hold "brans" constant in ruminant feed baskets
      # (not possible for pig system where brans are part of the main share):
      const[, , c("sys_beef.brans", "sys_dairy.brans")] <- 1
      anti[, , c("sys_beef.brans", "sys_dairy.brans")] <- 0
      out <- mbind(main, anti, const)

      if (!all(dimSums(out, dim = 3.3) == 1)) {
        stop("Something went wrong assigning commodities to types.
             Each commodity needs to be assigned to exactly one type!")
      }

      return(out)
    }

    # feed within the commodity group "const" is for all animal systems excluded
    # from the transition process (from mainShr to antiShr)
    ctype <- .belong2type(commodities = getNames(fbaskSys, dim = 2),
                          elems = getNames(fbaskSys))

    past <- findset("past_til2020")
    year <- tail(past, 1)

    # read in the ratio of livestock production allocated to the different systems
    prodSysRatio <- calcOutput("ProdSysRatioPast", aggregate = FALSE)
    prodSysRatio <- toolHoldConstantBeyondEnd(prodSysRatio)

    # use livestock production as weight
    kli       <- findset("kli")
    weightKLI <- collapseNames(calcOutput("FAOmassbalance_pre",
                                          aggregate = FALSE)[, past, kli][, , "dm"][, , "production"])
    weightSYS <- dimSums(fbaskSys, dim = 3.2)
    weightSYS[, , ] <- 0
    for (t in past) {
      weightSYS[, t, ] <- dimSums(weightKLI[, t, ] * prodSysRatio[, t, ], dim = 3.1)
    }
    weightSYS <- toolHoldConstantBeyondEnd(weightSYS)
    weightKLI <- setYears(weightKLI[, year, ], NULL)

    .calcFbaskShr <- function(x, main, dim) {

      out <- dimSums(x * main, dim = dim) / dimSums(x, dim = dim)
      if (anyNA(out)) {
        replacement <- as.magpie(apply(out, 3, mean, na.rm = TRUE))
        out <- toolNAreplace(out, replaceby = replacement)$x
      }
      return(out)
    }

    fbaskShr <- .calcFbaskShr(fbaskSys, ctype, dim = 3.2)

    # Read in Central Feedshares and calibrate them to observed shares
    outShr <- calcOutput("CentralFeedshares", aggregate = FALSE)


    .calcCalibShr <- function(fbaskShr, outShr, start_year, end_year, type) { # nolint
      # fill years
      fbaskShr <- toolHoldConstantBeyondEnd(fbaskShr)

      # calibration of main share:
      future <- getYears(fbaskShr)[-match(past, getYears(fbaskShr), nomatch = FALSE)]
      diffm  <- magpie_expand(fbaskShr[, year, "main",
                                       drop = TRUE][, , getNames(outShr, dim = 1)], outShr[, year, ]) - outShr[, year, ]
      outm   <- magpie_expand(fbaskShr[, , "main", drop = TRUE][, , getNames(outShr, dim = 1)], outShr)
      outm[, future, ] <- outShr[, future, ] + setYears(diffm, NULL)

      # convergence to regression values:
      outm <- convergence(outm, outShr, start_year = start_year, end_year = end_year, type = type)
      # set limit of 10% main share or the share in the last historical year in case it is lower than the limit
      outm[, future, ] <- outm[, future, ] * (outm[, future, ] > 0.1) + outm[, year, ] *
        (outm[, future, ] < 0.1 & outm[, year, ] < 0.1) + 0.1 * (outm[, future, ] < 0.1 & outm[, year, ] > 0.1)

      # add missing systems
      missing <- setdiff(getNames(fbaskShr, dim = 1), getNames(outm, dim = 1))
      outm    <- add_columns(outm, missing, dim = 3.1)
      outm[, , missing] <- fbaskShr[, , "main", drop = TRUE][, , missing]

      out <- add_dimension(fbaskShr, dim = 3.2, add = "scen", nm = getNames(outm, dim = 2))

      out[, , "main"] <- outm
      out[, , "anti"] <- 1 - out[, , "main"] - out[, , "const"]
      # remove negative values:
      out[, , "anti"][which(out[, , "anti"] < 0)] <- 0
      out[, , "const"] <- 1 - out[, , "main"] - out[, , "anti"]

      if (!all(round(dimSums(out, dim = "type"), 8) == 1)) {
        stop("Something went wrong calibrating the fbask shares!")
      }
      return(out)
    }
    calShr <- .calcCalibShr(fbaskShr, outShr,
                            start_year = year,
                            end_year = 2050, type = "linear")


    # Read in efficiencies and calibrate them
    outEff <- calcOutput("FeedEfficiencyFuture", aggregate = FALSE)
    .calcMultiplier <- function(x, year) {

      out <- x / setYears(x[, year, ], NULL)
      out[is.nan(out) | is.infinite(out)] <- 1
      return(out)
    }

    multEff  <- .calcMultiplier(outEff, year)

    .calcFshare <- function(fbaskSys, ctype, calShr, weightSYS, year) {
      .calcConvergeWeight <- function(x, years = NULL, start = year, converge = TRUE) {
        .calcWeight <- function(x, dim = 3.2) {
          x <- x + dimSums(x, dim = 1) / sum(x) * 10^-10
          return(x / dimSums(x, dim = dim))
        }
        w <- .calcWeight(x, 3.2)
        w <- setYears(w[, rep(1, length(years)), ], years)

        if (converge) {
          # convergence to global mean weights (assuming that production systems will get more similar over time)
          wglo <- magpie_expand(.calcWeight(dimSums(x * weightSYS[, getYears(x), ],
                                                    dim = 1) / dimSums(weightSYS[, getYears(x), ],
                                                                       dim = 1), 3.2), w)

          w <- convergence(w, wglo, start_year = start, end_year = tail(years, 1), type = "linear")
        }
        if (!all(round(dimSums(w, dim = 3.2), 8) == 1)) {
          stop("Something went wrong in the weight calculation (sum!=1)!")
        }
        return(w)
      }
      fbaskSysRef <- setYears(fbaskSys[, year, ], NULL)

      mainWeight  <- .calcConvergeWeight(years = getYears(calShr),
                                         start = year,
                                         x = fbaskSysRef * ctype[, , "main"],
                                         converge = FALSE)
      antiWeight  <- .calcConvergeWeight(years = getYears(calShr),
                                         start = year,
                                         x = fbaskSysRef * ctype[, , "anti"],
                                         converge = TRUE)
      # no convergence to global targets for systems
      # where the main feed share stays constant over time: "sys_chicken","sys_hen"
      tmp <- .calcConvergeWeight(start = year, years = getYears(calShr),
                                 x = (fbaskSysRef[, , c("sys_chicken", "sys_hen")]
                                      * ctype[, , c("sys_chicken", "sys_hen")][, , "anti"]),
                                 converge = FALSE)
      antiWeight[, , c("sys_chicken", "sys_hen")] <- tmp

      constWeight <- .calcConvergeWeight(start = year,
                                         years = getYears(calShr),
                                         x = fbaskSysRef * ctype[, , "const"],
                                         converge = FALSE)

      weight <- mbind(mainWeight, antiWeight, constWeight)
      fshare <- dimSums(weight * calShr, dim = 3.3) # problem with "type"
      fbaskSysCal <- fshare * dimSums(fbaskSysRef, dim = 3.2)

      if (any(round(fbaskSysCal[, year, ] - fbaskSys[, year, ], 9) != 0)) {
        stop("Something went wrong in the fbask calibration!")
      }

      return(fbaskSysCal)
    }
    fbaskSysCal <- .calcFshare(fbaskSys, ctype, calShr, weightSYS, year)

    ########### application of total feed efficiency trends and main feed share trends
    dataSys           <- fbaskSysCal * multEff
    dataSys[, past, ] <- fbaskSys

    # make sure that self-demand stays below a given threshold
    selfDemand <- c("sys_pig.livst_pig", "sys_beef.livst_rum",
                    "sys_chicken.livst_chick", "sys_hen.livst_egg",
                    "sys_dairy.livst_milk")
    threshold  <- 0.8
    if (any(dataSys[, , selfDemand] >= threshold)) {
      stop("Some systems were demanding more than ",
           threshold * 100, "% of itself as feed!")
    }

    # calculation of product-specific feed basket data (for livst_chick,livst_egg,livst_milk,livst_pig,livst_rum)
    data <- dimSums(prodSysRatio * dataSys, dim = 3.1)

    # remove non_eaten_food if not established as product yet
    if (!non_eaten_food) {
      data      <- data[, , "non_eaten_food", invert = TRUE]
      weightKLI <- weightKLI[, , "non_eaten_food", invert = TRUE]
    }

    getSets(data, fulldim = FALSE)[3] <- "kli.k.scen"

    return(list(x = data,
                weight = weightKLI,
                unit = "1",
                min = 0,
                description = "Detailed feed requirements in DM per DM products
                               generated for 5 livestock commodities"))
  }
}
