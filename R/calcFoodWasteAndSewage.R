# recycling of food waste and sewage?
calcFoodWasteAndSewage <- function(historic = TRUE) {
  past <- findset("past")
  mb <- calcOutput("FAOmassbalance", aggregate = FALSE)
  demand <- collapseNames(mb[, , "households"][, past, c("nr")])
  getSets(demand)[[3]] <- "ItemCodeItem"

  # food waste should be estimated without using demand projections

  scenarios <- readSource("Bodirsky2018", subtype = "scenarios")
  scenarios <- toolHoldConstantBeyondEnd(scenarios)
  intakeShare <- scenarios[, , "Intake (kcal/capita/day)"] / scenarios[, , "Demand for food (kcal/capita/day)"]
  intakeShare <- collapseNames(intakeShare)
  intakeShare[intakeShare > 1] <- 1

  if (historic) {
    intakeShare <- collapseNames(intakeShare[, getYears(demand), "SSP2"])
  } else {
    ### demand calculation could also move into a specific function.

    kap <- findset("kap")
    processed <- c("sugar", "oils", "alcohol")
    kst <- findset("kst")
    future <- setdiff(getYears(scenarios), getYears(demand))
    lastyear <- paste0("y", max(getYears(demand, as.integer = TRUE)))

    demand <- add_dimension(demand, dim = 3.1, add = "scenario", nm = "SSP2")
    demand <- add_columns(demand, dim = 3.1, addnm = c("SSP1", "SSP3", "SSP4", "SSP5"))
    demand[, , ] <- demand[, , "SSP2"]
    demand <- toolHoldConstantBeyondEnd(demand)

    demandAnimal <- "Demand for animal source foods (kcal/capita/day)"
    futureAnimalFactor <- scenarios[, future, demandAnimal] / setYears(scenarios[, lastyear, demandAnimal], NULL)
    demand[, future, kap]       <- demand[, future, kap]       * collapseNames(futureAnimalFactor)

    demandEmpty <- "Demand for empty calories (kcal/capita/day)"
    futureEmptyFactor <- scenarios[, future, demandEmpty] / setYears(scenarios[, lastyear, demandEmpty], NULL)
    demand[, future, processed] <- demand[, future, processed] * collapseNames(futureEmptyFactor)

    demandFood <- "Demand for food (kcal/capita/day)"
    futureFoodFactor <- scenarios[, future, demandFood] / setYears(scenarios[, lastyear, demandFood], NULL)
    demand[, future, kst]       <- demand[, future, kst]       * collapseNames(futureFoodFactor)

    demandVeg <- "Demand for vegetables fruits and nuts (kcal/capita/day)"
    futureVegFactor <- scenarios[, future, demandVeg] / setYears(scenarios[, lastyear, demandVeg], NULL)
    demand[, future, "others"]  <- demand[, future, "others"]  * collapseNames(futureVegFactor)
  }

  # assuming N:P ratio of 10:1 based on Moree et al (2013)
  # Moree, A. L., A. H. W. Beusen, A. F. Bouwman, and W. J. Willems. 2013. “Exploring Global
  # Nitrogen and Phosphorus Flows in Urban Wastes during the Twentieth Century.” Global
  # Biogeochemical Cycles 27 (3): 836–46. doi:10.1002/gbc.20072.
  demand <- mbind(add_dimension(demand, dim = 3.1, nm = "nr"), add_dimension(demand, dim = 3.1, nm = "p") / 10)
  # not differentiated by product
  demand <- dimSums(demand, dim = "ItemCodeItem")

  # urine:80% of intake for N, 62% for P (moree)
  urine <- new.magpie("GLO", NULL, c("nr", "p"))
  urine[, , "nr"] <- 0.8
  urine[, , "p"] <- 0.62

  out <- mbind(
    add_dimension(demand * (1 - intakeShare), dim = 3.1, nm = "hh_food_waste"),
    add_dimension(demand * (intakeShare) * urine, dim = 3.1, nm = "urine"),
    add_dimension(demand * (intakeShare) * (1 - urine), dim = 3.1, nm = "feces")
  )

  # add recycling as feed, fertilizer

  return(list(x = out,
              weight = NULL,
              unit = "Mt Nr, P",
              description = "Further destiny of food")
  )
}
