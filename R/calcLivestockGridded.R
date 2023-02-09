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
#' @importFrom magpiesets findset

calcLivestockGridded <- function(details = FALSE) {

  selectyears <- findset("past")

  countryToCell       <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")
  livestockProduction <- calcOutput("FAOmassbalance", aggregate = FALSE)[, selectyears, "production"]

  ### ruminants

  ruminants     <- c("livst_milk", "livst_rum")

  # Divide ruminants in extensive and intensive depending on feedmix
  feedPast      <- calcOutput("FeedPast", nutrients = "nr", aggregate = FALSE)
  ruminantFeed  <- collapseNames(feedPast[, selectyears, c("alias_livst_milk", "alias_livst_rum")])
  cropbasedFeed <- dimSums(ruminantFeed[, , "pasture", invert = TRUE], dim = 3.2) / dimSums(ruminantFeed, dim = 3.2)
  pastbasedFeed <- collapseNames(ruminantFeed[, , "pasture"]) / dimSums(ruminantFeed, dim = 3.2)
  cropbasedFeed[is.na(cropbasedFeed)] <- 0
  pastbasedFeed[is.na(pastbasedFeed)] <- 0

  getNames(pastbasedFeed) <- getNames(cropbasedFeed) <- ruminants

  ruminantProduction    <- collapseNames(livestockProduction[, , ruminants])
  extensiveRuminant     <- ruminantProduction * cropbasedFeed
  intensiveRuminant     <- ruminantProduction * pastbasedFeed

  # calculate extensive ruminant production per cell from pasture production share
  pastureProduction     <- collapseNames(calcOutput("Production", products = "pasture", cellular = TRUE,
                                                    calibrated = TRUE, aggregate = FALSE)[, selectyears, "nr"])
  extensiveRuminantCell <- toolAggregate(toolIso2CellCountries(extensiveRuminant), rel = countryToCell,
                                         weight = pastureProduction, from = "iso", to = "celliso", dim = 1)

  # calculate intensive ruminant production per cell from cropland share
  kcrProduction <- toolCell2isoCell(calcOutput("Production", products = "kcr", cellular = TRUE, aggregate = FALSE))
  kcrProduction <- kcrProduction[, selectyears, "dm"][, , c("betr", "begr"), invert = TRUE]
  cropProduction        <- dimSums(collapseNames(kcrProduction), dim = 3)
  intensiveRuminantCell <- toolAggregate(toolIso2CellCountries(intensiveRuminant), rel = countryToCell,
                                         weight = cropProduction, from = "iso", to = "celliso", dim = 1)

  ruminantProdCell      <- extensiveRuminantCell + intensiveRuminantCell

  ### poultry and pig

  poultry               <- c("livst_chick", "livst_egg")
  pig                   <- "livst_pig"

  # Divide pigs and poultry in extensive and intensive depending on development state

  developmentState      <- calcOutput("developmentState", aggregate = FALSE)
  developmentState      <- setNames(collapseNames(developmentState[, selectyears, "SSP2"]), nm = poultry[1])
  developmentState      <- mbind(developmentState, setNames(developmentState, nm = poultry[2]))
  upper                 <- calcOutput("developmentState", upper = 30000, aggregate = FALSE)
  developmentState      <- mbind(developmentState, setNames(collapseNames(upper[, selectyears, "SSP2"]), nm = pig))

  pigPoultryProduction  <- collapseNames(livestockProduction[, , c(pig, poultry)])
  extensivePigPoultry   <- pigPoultryProduction * (1 - developmentState)
  intensivePigPoultry   <- pigPoultryProduction * developmentState

  # calculate extensive poultry and pig production per cell from urbanarea share
  landuseInitialization   <- calcOutput("LanduseInitialisation", cellular = TRUE, aggregate = FALSE)
  urbanarea               <- toolCell2isoCell(landuseInitialization[, selectyears, "urban"])
  extensivePigPoultryCell <- toolAggregate(toolIso2CellCountries(extensivePigPoultry), rel = countryToCell,
                                           weight = urbanarea, from = "iso", to = "celliso", dim = 1)

  # calculate intensive pig poultry production per cell from cropland share
  # more ideas to come for pig poultry disaggregation
  intensivePigPoultryCell <- toolAggregate(toolIso2CellCountries(intensivePigPoultry), rel = countryToCell,
                                           weight = cropProduction, from = "iso", to = "celliso", dim = 1)

  pigPoultryProdCell      <- extensivePigPoultryCell + intensivePigPoultryCell

  ### Total Livestock

  if (details == FALSE) {

    magProduction           <- mbind(ruminantProdCell, pigPoultryProdCell)

  } else if (details == TRUE) {

    magProduction           <- mbind(add_dimension(extensiveRuminantCell, dim = 3.1, add = "intensity", nm = "ext"),
                                     add_dimension(intensiveRuminantCell, dim = 3.1, add = "intensity", nm = "int"),
                                     add_dimension(extensivePigPoultryCell, dim = 3.1, add = "intensity", nm = "ext"),
                                     add_dimension(intensivePigPoultryCell, dim = 3.1, add = "intensity", nm = "int"))
  }


  return(list(x = magProduction,
              weight = NULL,
              unit = "Mt DM/Nr/P/K/WM or PJ energy",
              description = paste("Cellular livestock production: dry matter: Mt (dm),",
                                  "gross energy: PJ (ge), reactive nitrogen: Mt (nr),",
                                  "phosphor: Mt (p), potash: Mt (k), wet matter: Mt (wm)."),
              min = -Inf,
              max = Inf,
              isocountries = FALSE))
}
