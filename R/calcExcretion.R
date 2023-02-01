#' @title calcExcretion
#' @description calculates excretion during grazing, cropland-grazing, confinement and collected for fuel.
#' Based on MAgPIE Feed baskets, slaughter biomass and simple allocation rules.
#'
#' @param cellular if TRUE value is calculate and returned (set aggregate to FALSE!) on cellular level
#' @param attributes npk (default) or npkc (inclusing carbon) can be selected
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcExcretionIPCC()]
#' @examples
#' \dontrun{
#' calcOutput("Excretion")
#' }
#' @importFrom magclass getNames<-
calcExcretion <- function(cellular = FALSE, attributes = "npk") {
  # read in sets
  nutrients  <- c("nr", "p", "k")
  past       <- findset("past")
  kres       <- findset("kres")
  kli2       <- findset(set = "kli", alias = TRUE)
  kli        <- findset(set = "kli")

  # read in inputs

  developmentState <- calcOutput("DevelopmentState", aggregate = FALSE)
  croplandGrazingShare         <- collapseNames(1 - developmentState[, past, "SSP2"]) * 0.25
  fuelShare                    <- collapseNames(calcOutput("ManureFuelShr", aggregate = FALSE)[, past, "SSP2"])
  getNames(fuelShare, dim = 1) <- paste0("alias_", getNames(fuelShare, dim = 1))


  feed <- calcOutput("FeedPast", balanceflow = TRUE, aggregate = FALSE, nutrients = nutrients)[, past, ][, , kli2]

  # calculate feeding categories
  leftOnPasture            <- collapseNames(feed[, , "pasture"] * (1 - fuelShare))
  removedForFuel           <- collapseNames(feed[, , "pasture"] * (fuelShare))
  leftOnCropland           <- dimSums(feed[, , kres] * croplandGrazingShare, dim = 3.2)
  feedTotal                <- dimSums(feed, dim = 3.2)


  confinement              <- feedTotal - leftOnPasture - removedForFuel - leftOnCropland

  feedingSystems           <- mbind(add_dimension(leftOnPasture,  dim = 3.1, nm = "grazing"),
                                    add_dimension(removedForFuel, dim = 3.1, nm = "fuel"),
                                    add_dimension(leftOnCropland, dim = 3.1, nm = "stubble_grazing"),
                                    add_dimension(confinement,    dim = 3.1, nm = "confinement"))

  slaughterFeedShare <- calcOutput("SlaughterFeedShare", aggregate = FALSE, balanceflow = TRUE)
  slaughterFeedShare <- collapseNames(slaughterFeedShare[, past, nutrients][, , "constant"][, , kli])
  getNames(slaughterFeedShare, dim = 1) <- paste0("alias_", getNames(slaughterFeedShare, dim = 1))

  excretion <- feedingSystems * (1 - slaughterFeedShare)

  excretion[is.na(excretion)] <- 0
  getNames(excretion, dim = 2) <- substring(getNames(excretion, dim = 2), 7)


  if (cellular) {
    livestockProduction <- collapseNames(calcOutput("LivestockGridded", details = TRUE, aggregate = FALSE)[, , "dm"])

    productionWeights   <- new.magpie(getItems(livestockProduction, dim = 1),
                                      getItems(livestockProduction, dim = 2),
                                      outer(getNames(excretion, dim = 1),
                                            getNames(excretion, dim = 2), paste, sep = "."),
                                      fill = 0)

    livstRumMilk <- c("livst_rum", "livst_milk")
    productionWeights[, , "grazing"][, , livstRumMilk]          <- livestockProduction[, , "ext"][, , livstRumMilk]
    productionWeights[, , "fuel"][, , livstRumMilk]             <- livestockProduction[, , "ext"][, , livstRumMilk]
    productionWeights[, , "stubble_grazing"][, , livstRumMilk]  <- livestockProduction[, , "int"][, , livstRumMilk]
    productionWeights[, , "confinement"][, , livstRumMilk]      <- livestockProduction[, , "int"][, , livstRumMilk]
    livstChickEggPig <- c("livst_chick", "livst_egg", "livst_pig")
    productionWeights[, , livstChickEggPig] <- dimSums(livestockProduction[, , livstChickEggPig], dim = 3.1)


    mapping <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")
    mapping <- mapping[which(mapping$iso %in% getItems(livestockProduction, dim = 1.1)), ]
    excretion <- toolAggregate(excretion[getItems(livestockProduction, dim = 1.1), , ],
                               rel = mapping, weight = productionWeights, from = "iso", to = "celliso", dim = 1)
  }
  excretion <- round(excretion, 8)

  if (attributes == "npkc") {
    cnRatio <- collapseNames(readSource("IPCC", subtype = "manure_table5p5c", convert = FALSE)[, , "cn_ratio"])
    excretion <- add_columns(excretion, addnm = "c", dim = 3.3)
    excretion[, , "c"] <- excretion[, , "nr"] * cnRatio
    unit <- "Mt Nr, P, K , C"
  } else {
    unit <- "Mt Nr, P, K"
  }

  return(list(x = excretion,
              weight = NULL,
              unit = unit,
              min = 0,
              description = "Excreted nutrients per animal type and animal waste system",
              isocountries = !cellular))
}
