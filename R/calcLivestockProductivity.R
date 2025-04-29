#' Calculate Yields for Livestock
#' @description
#' Provides MAgPIE-FEED data for livestock-yields calculated in the regression
#' for feed (calcRegressionFEED).. No changes to the content have been done,
#' besides renaming and providing weights.
#'
#'
#' @param future if TRUE calculates Constant future and linear trends based on SSP Expert guesses
#' @return MAgPIE-FEED data for livestock-yields and corresonding weights as a
#' list of two MAgPIE objects
#' @author Isabelle Weindl, Lavinia Baumstark, Stephen Wirth
#' @seealso [madrat::calcOutput()], [madrat::readSource()]
#' @examples
#' \dontrun{
#' calcOutput("LivestockProductivity")
#' }
#' @importFrom magclass getNames clean_magpie add_dimension
#' @importFrom luscale rename_dimnames

calcLivestockProductivity <- function(future = TRUE) {

  stock <- readSource("FAO_online", subtype = "LiveHead")
  prim  <- readSource("FAO_online", subtype = "LivePrim")

  # separate FAO variable number
  getNames(stock) <- gsub("\\|", ".", getNames(stock))
  getNames(prim)  <- gsub("\\|", ".", getNames(prim))

  # calculate stockYield
  types <- c("Pigs", "Cattle", "Chickens")
  names(types) <- c("Meat, pig", "Meat, cattle", "Meat, chicken")
  # magpie object of 10 time steps for meat from cattle, chicken and pigs
  x1 <- setNames(prim[, , "production"][, , names(types)], unname(types))
  # magpie object of 10 time steps for animal numbers for cattle, chicken and pigs
  x2 <- collapseNames(stock[, , types, drop = TRUE], collapsedim = 1)

  tmp <- intersect(getYears(x1), getYears(x2))
  out <- toolNAreplace(x1[, tmp, ] / x2[, tmp, ], x2[, tmp, ], replaceby = dimSums(x1[, tmp, ], dim = 1) /
                         dimSums(x2[, tmp, ], dim = 1), val.rm = 0)
  stockYield <- out$x
  weight      <- out$weight
  getNames(stockYield) <- paste0("stock_yield.", getNames(stockYield))
  getNames(weight) <- paste0("stock_yield.", getNames(weight))

  # calculate prodYield
  types <- c("Eggs", "Milk")
  x1 <- setNames(prim[, , "production"][, , c("Eggs, hen, in shell", "Milk, whole fresh cow")], types)
  x2 <- setNames(prim[, , c("1062.Eggs, hen, in shell.Laying_(Head)",
                            "882.Milk, whole fresh cow.Milk_Animals_(Head)")], types)
  # prim prod sum / prod head
  out <- toolNAreplace(x1 / x2, x2, replaceby = dimSums(x1, dim = 1) / dimSums(x2, dim = 1), val.rm = 0)
  prodYield <- out$x
  weight2    <- out$weight
  getNames(prodYield) <- paste0("producer_yield.", getNames(prodYield))
  getNames(weight2) <- paste0("producer_yield.", getNames(weight2))

  # put both into one magpie
  weight <- mbind(weight, weight2)
  yield <- mbind(stockYield[, tmp, ], prodYield[, tmp, ])

  getNames(yield) <- getNames(yield, dim = 2)
  getNames(weight) <- getNames(weight, dim = 2)
  mapping <- data.frame(
    groups = c("Pigs", "Cattle", "Chickens", "Eggs", "Milk"),
    sys = c("sys_pig", "sys_beef", "sys_chicken", "sys_hen", "sys_dairy"), stringsAsFactors = FALSE
  )

  yield <- rename_dimnames(yield, dim = 3, query = mapping, from = "groups", to = "sys")
  weight <- rename_dimnames(weight, dim = 3, query = mapping, from = "groups", to = "sys")

  # preparing output for magpie time steps "past"
  past <- findset("past")
  yieldPast <- yield[, past, ]
  weightPast <- weight[, past, ]

  if (future == FALSE) {
    return(list(
      x = yieldPast,
      weight = weightPast,
      unit = c("t Fresh matter per animal"),
      description = "livestock productivity (yield) as stock (meat producers) or producer (dairy/egg) yield"
    ))
  } else if (future == TRUE) {
    histYield <- clean_magpie(yield, what = "sets")

    # extrapolate the trends to 2020 to reduce the number of time steps filled by the expertGuess-approach below
    # select two 5-year averages for the 2 time steps to be used for extrapolation
    average <- 5
    dt <- floor(average / 2)
    year1 <- tail(getYears(histYield), n = average + dt + 1)[1]
    year2 <- tail(getYears(histYield), n = dt + 1)[1]
    exyears <- c(year1, year2)
    exyears <- as.numeric(gsub("y", "", exyears))

    dataset <- mbind(toolTimeAverage(histYield[, seq(exyears[1] - dt, exyears[1] + dt), ], average),
                     toolTimeAverage(histYield[, seq(exyears[2] - dt, exyears[2] + dt), ], average))
    extra2020 <- time_interpolate(dataset = dataset,
                                  interpolated_year = 2020,
                                  integrate_interpolated_years = FALSE,
                                  extrapolation_type = "linear")

    for (i in getNames(histYield)) {
      extra2020[, , i] <- toolConditionalReplace(extra2020[, , i], "<0", min(histYield[, , i]))
    }

    histYield <- mbind(histYield, extra2020)

    # selecting data for years included in magpie time steps "time"
    magYears <- findset("time")
    histYield <- histYield[, intersect(getYears(histYield), magYears), ]
    weight <- weight[, intersect(getYears(weight), magYears), ]

    outputConstant <- toolHoldConstantBeyondEnd(histYield)
    weight <- toolHoldConstantBeyondEnd(weight)


    expertGuess <- function(histYield) {
      # Expert guesses are a number-coded matrix to indicate magnitude of future yield gain
      # 1low
      # 2medium
      # 3high
      # 4catch-up
      # 5exceptional

      yieldScen <- readSource("ExpertGuessSSPLivestockProduction", "ssp1")
      yieldScen <- add_dimension(yieldScen, dim = 3.2, add = "scen", nm = "ssp1")

      for (i in 2:5) {
        scen <- paste("ssp", i, sep = "")
        yieldScen <- add_columns(yieldScen, dim = 3.2, addnm = scen)
        yieldScen[, , scen] <- readSource("ExpertGuessSSPLivestockProduction", scen)
      }

      scenario <- scenario2 <- yieldScen
      scenario2[, , ] <- NA

      # BEEF
      animal <- "sys_beef"
      # low
      scenario2[, , animal][which(scenario[, , animal] == 1)] <- 0.0002
      # medium
      scenario2[, , animal][which(scenario[, , animal] == 2)] <- 0.0004
      # high
      scenario2[, , animal][which(scenario[, , animal] == 3)] <- 0.0008
      # catch-up
      scenario2[, , animal][which(scenario[, , animal] == 4)] <- 0.0016

      # MILK
      animal <- "sys_dairy"
      # low
      scenario2[, , animal][which(scenario[, , animal] == 1)] <- 0.02
      # medium
      scenario2[, , animal][which(scenario[, , animal] == 2)] <- 0.04
      # high
      scenario2[, , animal][which(scenario[, , animal] == 3)] <- 0.06
      # exceptional
      scenario2[, , animal][which(scenario[, , animal] == 5)] <- 0.12

      # Chicken
      animal <- "sys_chicken"
      # low
      scenario2[, , animal][which(scenario[, , animal] == 1)] <- 0.00001
      # medium
      scenario2[, , animal][which(scenario[, , animal] == 2)] <- 0.00004
      # high
      scenario2[, , animal][which(scenario[, , animal] == 3)] <- 0.0001

      # Eggs
      animal <- "sys_hen"
      # low
      scenario2[, , animal][which(scenario[, , animal] == 1)] <- 0.04 * 0.001
      # high
      scenario2[, , animal][which(scenario[, , animal] == 3)] <- 0.08 * 0.001
      # catch-up
      scenario2[, , animal][which(scenario[, , animal] == 4)] <- 0.14 * 0.001

      # Pork
      animal <- "sys_pig"
      # low
      scenario2[, , animal][which(scenario[, , animal] == 1)] <- 0.0004
      # high
      scenario2[, , animal][which(scenario[, , animal] == 3)] <- 0.001
      # catch-up
      scenario2[, , animal][which(scenario[, , animal] == 4)] <- 0.0016

      productivity <- add_dimension(histYield, dim = 3.2, add = "scen", nm = paste0("ssp", 1:5))
      productivity <- add_columns(productivity, addnm = c("y2030", "y2050", "y2100"), dim = 2.1)
      productivity[, c("y2030", "y2050", "y2100"), ] <- setYears(productivity[, c("y2020"), ], NULL)
      productivity[, c("y2030", "y2050", "y2100"), ] <- productivity[, c("y2030", "y2050", "y2100"), ] +
        setYears(scenario2[, "y2030", ], NULL) * 10
      productivity[, c("y2050", "y2100"), ] <- productivity[, c("y2050", "y2100"), ] + setYears(scenario2[, "y2050", ],
                                                                                                NULL) * 20
      productivity[, c("y2100"), ] <- productivity[, c("y2100"), ] + setYears(scenario2[, "y2100", ], NULL) * 50
      productivity <- time_interpolate(dataset = productivity, interpolated_year = 2020 + (1:26) * 5,
                                       integrate_interpolated_years = TRUE, extrapolation_type = "linear")
      return(productivity)
    }

    output <- expertGuess(histYield = histYield)
    output <- add_columns(output, addnm = "constant", dim = 3.2)
    output[, , "constant"] <- outputConstant

    return(list(
      x = output,
      weight = weight,
      unit = c("t Fresh matter per animal"),
      description = "livestock productivity (yield) as stock (meat producers) or producer (dairy/egg) yield"
    ))
  }
}
