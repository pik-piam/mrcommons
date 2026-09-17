#' @title calcLivestockGridded
#' @description Distributes crop, pasture and livestock production in space to 0.5 degree
#'
#' @param details switch, if set to TRUE will lead to reporting of extensive and intensive livestock shares
#' @return List of magpie objects with results on cellular level, weights on cellular level, unit and description.
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#' calcOutput("calcLivestockGridded")
#' }
#'

calcLivestockGridded <- function(details = FALSE) {

  countryToCell <- toolGetMappingCoord2Country()
  countryToCell$coordiso <- paste(countryToCell$coords, countryToCell$iso, sep = ".")

  # country-level livestock production
  livestockProduction <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , "production"]

  # ruminant categories
  ruminants     <- c("livst_milk", "livst_rum")

  # Divide ruminants in extensive and intensive depending on feedmix
  feedPast            <- calcOutput("FeedPast", nutrients = "nr", aggregate = FALSE)
  commonYears         <- intersect(getYears(feedPast), getYears(livestockProduction))
  feedPast            <- feedPast[, commonYears, ]
  livestockProduction <- livestockProduction[, commonYears, ]

  ruminantFeed  <- collapseNames(feedPast[, , c("alias_livst_milk", "alias_livst_rum")])
  cropbasedFeed <- dimSums(ruminantFeed[, , "pasture", invert = TRUE], dim = 3.2) / dimSums(ruminantFeed, dim = 3.2)
  pastbasedFeed <- collapseNames(ruminantFeed[, , "pasture"]) / dimSums(ruminantFeed, dim = 3.2)
  cropbasedFeed[is.na(cropbasedFeed)] <- 0
  pastbasedFeed[is.na(pastbasedFeed)] <- 0

  getNames(pastbasedFeed) <- getNames(cropbasedFeed) <- ruminants

  ruminantProduction    <- collapseNames(livestockProduction[, , ruminants])
  extensiveRuminant     <- ruminantProduction * cropbasedFeed
  intensiveRuminant     <- ruminantProduction * pastbasedFeed

  # calculate extensive ruminant production per cell from pasture production share
  pastureProduction     <- collapseNames(calcOutput("Production", products = "pasture",
                                                    cellular = TRUE,
                                                    calibrated = TRUE, aggregate = FALSE)[, , "nr"])

  commonYears   <- intersect(getYears(pastureProduction), getYears(livestockProduction))
  livestockProduction <- livestockProduction[, commonYears, ]
  pastureProduction   <- pastureProduction[, commonYears, ]
  extensiveRuminant   <- extensiveRuminant[, commonYears, ]
  intensiveRuminant   <- intensiveRuminant[, commonYears, ]

  countries <- getItems(pastureProduction, dim = 1.3)
  extensiveRuminantCell <- toolAggregate(extensiveRuminant[countries, , ], rel = countryToCell,
                                         weight = pastureProduction + 10^(-10),
                                         from = "iso", to = "coordiso", dim = 1)

  # calculate intensive ruminant production per cell from cropland share
  kcrProduction <- calcOutput("Production", products = "kcr",
                              cellular = TRUE, aggregate = FALSE)
  kcrProduction <- kcrProduction[, , "dm"][, , c("betr", "begr"), invert = TRUE]

  commonYears   <- intersect(getYears(kcrProduction), getYears(livestockProduction))
  kcrProduction <- kcrProduction[, commonYears, ]
  extensiveRuminantCell <- extensiveRuminantCell[, commonYears, ]
  intensiveRuminant     <- intensiveRuminant[, commonYears, ]
  livestockProduction   <- livestockProduction[, commonYears, ]

  cropProduction        <- dimSums(collapseNames(kcrProduction), dim = 3)
  intensiveRuminantCell <- toolAggregate(intensiveRuminant[countries, , ], rel = countryToCell,
                                         weight = cropProduction + 10^(-10),
                                         from = "iso", to = "coordiso", dim = 1)

  ruminantProdCell      <- extensiveRuminantCell + intensiveRuminantCell

  ### poultry and pig categories
  poultry               <- c("livst_chick", "livst_egg")
  pig                   <- "livst_pig"

  # Divide pigs and poultry in extensive and intensive depending on development state
  developmentState      <- calcOutput("DevelopmentState", aggregate = FALSE)
  developmentState      <- setNames(collapseNames(developmentState[countries, commonYears, "SSP2"]), nm = poultry[1])
  developmentState      <- mbind(developmentState, setNames(developmentState, nm = poultry[2]))
  upper                 <- calcOutput("DevelopmentState", upper = 30000, aggregate = FALSE)
  developmentState      <- mbind(developmentState,
                                 setNames(collapseNames(upper[countries, commonYears, "SSP2"]),
                                          nm = pig))

  pigPoultryProduction  <- collapseNames(livestockProduction[countries, , c(pig, poultry)])
  extensivePigPoultry   <- pigPoultryProduction * (1 - developmentState)
  intensivePigPoultry   <- pigPoultryProduction * developmentState

  # calculate extensive poultry and pig production per cell from urbanarea share
  landuseInitialization   <- calcOutput("LanduseInitialisation", cellular = TRUE,
                                        selectyears = commonYears,
                                        cells = "lpjcell", aggregate = FALSE)
  urbanarea               <- landuseInitialization[, , "urban"]
  extensivePigPoultryCell <- toolAggregate(extensivePigPoultry, rel = countryToCell,
                                           weight = urbanarea + 10^(-10),
                                           from = "iso", to = "coordiso", dim = 1)

  # calculate intensive pig poultry production per cell from cropland share
  # more ideas to come for pig poultry disaggregation
  intensivePigPoultryCell <- toolAggregate(intensivePigPoultry, rel = countryToCell,
                                           weight = cropProduction + 10^(-10),
                                           from = "iso", to = "coordiso", dim = 1)

  pigPoultryProdCell      <- extensivePigPoultryCell + intensivePigPoultryCell

  ### Total Livestock
  if (details) {
    x <- mbind(add_dimension(extensiveRuminantCell, dim = 3.1, add = "intensity", nm = "ext"),
               add_dimension(intensiveRuminantCell, dim = 3.1, add = "intensity", nm = "int"),
               add_dimension(extensivePigPoultryCell, dim = 3.1, add = "intensity", nm = "ext"),
               add_dimension(intensivePigPoultryCell, dim = 3.1, add = "intensity", nm = "int"))
    getSets(x) <- c("x", "y", "iso", "year", "intensity", "ItemCodeItem", "attributes")
  } else {
    x <- mbind(ruminantProdCell, pigPoultryProdCell)
    getSets(x) <- c("x", "y", "iso", "year", "ItemCodeItem", "attributes")
  }

  # Check for NAs and negatives
  if (any(round(x, digits = 4) < 0)) {
    stop("calcProduction produced negative values")
  }
  if (any(is.na(x))) {
    stop("calcProduction produced NA values")
  }

  return(list(x = x,
              weight = NULL,
              unit = "Mt DM/Nr/P/K/WM or PJ energy",
              description = paste("Cellular livestock production: dry matter: Mt (dm),",
                                  "gross energy: PJ (ge), reactive nitrogen: Mt (nr),",
                                  "phosphor: Mt (p), potash: Mt (k), wet matter: Mt (wm)."),
              min = -Inf,
              max = Inf,
              isocountries = FALSE))
}
