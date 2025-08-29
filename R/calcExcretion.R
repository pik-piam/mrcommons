#' @title calcExcretion
#' @description calculates excretion during grazing, cropland-grazing,
#'              confinement and collected for fuel.
#'              Based on MAgPIE Feed baskets, slaughter biomass and
#'              simple allocation rules.
#'
#' @param cellular   if TRUE value is calculate and returned (set aggregate to FALSE!) on cellular level
#' @param cells      Switch between "magpiecell" (59199) and "lpjcell" (67420)
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

calcExcretion <- function(cellular = FALSE, cells = "lpjcell", attributes = "npk") {
  # read in sets

  # read in sets
  nutrients  <- c("nr", "p", "k")
  kres       <- findset("kres")
  kli2       <- findset(set = "kli", alias = TRUE)
  kli        <- findset(set = "kli")

  # read in inputs
  developmentState             <- calcOutput("DevelopmentState", aggregate = FALSE)
  croplandGrazingShare         <- collapseNames(1 - developmentState[, , "SSP2"]) * 0.25
  fuelShare                    <- collapseNames(calcOutput("ManureFuelShr", aggregate = FALSE)[, , "SSP2"])
  getNames(fuelShare, dim = 1) <- paste0("alias_", getNames(fuelShare, dim = 1))

  feed <- calcOutput("FeedPast", balanceflow = TRUE, aggregate = FALSE, nutrients = nutrients)[, , ][, , kli2]

  commonYears          <- intersect(intersect(getYears(croplandGrazingShare),
                                              getYears(fuelShare)), getYears(feed))
  croplandGrazingShare <- croplandGrazingShare[, commonYears, ]
  fuelShare            <- fuelShare[, commonYears, ]
  feed                 <- feed[, commonYears, ]

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

  commonYears        <- intersect(getYears(slaughterFeedShare), getYears(feedingSystems))
  slaughterFeedShare <- slaughterFeedShare[, commonYears, ]
  feedingSystems     <- feedingSystems[, commonYears, ]

  slaughterFeedShare <- collapseNames(slaughterFeedShare[, commonYears, nutrients][, , "constant"][, , kli])
  getNames(slaughterFeedShare, dim = 1) <- paste0("alias_", getNames(slaughterFeedShare, dim = 1))

  excretion <- feedingSystems * (1 - slaughterFeedShare)

  excretion[is.na(excretion)]  <- 0
  getNames(excretion, dim = 2) <- substring(getNames(excretion, dim = 2), 7)



  if (cellular) {

    livestockProduction <- collapseNames(calcOutput("LivestockGridded", details = TRUE, aggregate = FALSE)[, , "dm"])

    commonYears          <- intersect(getYears(excretion), getYears(livestockProduction))
    excretion            <- excretion[, commonYears, ]
    livestockProduction  <- livestockProduction[, commonYears, ]

    productionWeights   <- new.magpie(cells_and_regions = getItems(livestockProduction, dim = 1),
                                      years = getItems(livestockProduction, dim = 2),
                                      names = outer(getNames(excretion, dim = 1),
                                                    getNames(excretion, dim = 2), paste, sep = "."),
                                      fill = 0,
                                      sets = c(getSets(livestockProduction, fulldim = FALSE)[1:2],
                                               "source.kli"))

    livstRumMilk <- c("livst_rum", "livst_milk")
    productionWeights[, , "grazing"][, , livstRumMilk]          <- livestockProduction[, , "ext"][, , livstRumMilk]
    productionWeights[, , "fuel"][, , livstRumMilk]             <- livestockProduction[, , "ext"][, , livstRumMilk]
    productionWeights[, , "stubble_grazing"][, , livstRumMilk]  <- livestockProduction[, , "int"][, , livstRumMilk]
    productionWeights[, , "confinement"][, , livstRumMilk]      <- livestockProduction[, , "int"][, , livstRumMilk]
    livstChickEggPig <- c("livst_chick", "livst_egg", "livst_pig")
    productionWeights[, , livstChickEggPig] <- dimSums(livestockProduction[, , livstChickEggPig], dim = 3.1)

    mapping <- toolGetMappingCoord2Country()
    mapping$coordiso <- paste(mapping$coords, mapping$iso, sep = ".")
    excretion <- toolAggregate(excretion[getItems(livestockProduction, dim = 1.3), , ],
                               rel = mapping, weight = productionWeights + 10^(-10),
                               from = "iso", to = "coordiso", dim = 1)
  }
  excretion <- round(excretion, digits = 8)

  if (attributes == "npkc") {
    cnRatio   <- collapseNames(readSource("IPCC", subtype = "manure_table5p5c", convert = FALSE)[, , "cn_ratio"])
    excretion <- add_columns(excretion, addnm = "c", dim = 3.3)
    excretion[, , "c"] <- excretion[, , "nr"] * cnRatio
    unit <- "Mt Nr, P, K , C"
  } else {
    unit <- "Mt Nr, P, K"
  }

  if (cellular) {
    if (cells == "magpiecell") {
      excretion <- toolCoord2Isocell(excretion, cells = cells)
    }
  }

  return(list(x = excretion,
              weight = NULL,
              unit = unit,
              min = 0,
              description = "Excreted nutrients per animal type and animal waste system",
              isocountries = !cellular))
}
