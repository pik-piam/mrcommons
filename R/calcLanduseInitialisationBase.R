#' @title calcLanduseInitialisationBase
#' @description Calculates the cellular MAgPIE landuse initialisation area. Data from FAO on forestry is used
#' to split the secondary forest pool of the LU2v2 dataset into forestry and secd_forest. This function
#' returns the data set in a basic configuration. Use \code{\link{calcLanduseInitialisation}} for
#' more settings.
#'
#' @param cells "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @param selectyears Years to be computed (default on "past")
#' @return Cellular landuse initialisation in its base configuration
#' @author Jan Philipp Dietrich, Benjamin Leon Bodirsky, Kristine Karstens, Felcitas Beier, Patrick v. Jeetze
#' @examples
#' \dontrun{
#' calcOutput("LanduseInitialisationBase")
#' }
#'
calcLanduseInitialisationBase <- function(cells = "magpiecell", selectyears = "past") {
  selectyears <- sort(findset(selectyears, noset = "original"))

  .luIni <- function(luh, forestArea) {
    .shr <- function(x) {
      x <- x + 10^-10
      return(x / dimSums(x, dim = 3))
    }

    .expand <- function(x, target) {
      map <- data.frame(from = getItems(target, dim = 1.1, full = TRUE),
                        to = getItems(target, dim = 1))
      return(toolAggregate(x[getItems(target, dim = 1.1), , ], map, from = "from", to = "to"))
    }
    map <- data.frame(luh = c("c3ann", "c4ann", "c3per", "c4per", "c3nfx", "pastr", "range",
                                   "primf",      "secdf",    "secdf", "urban",     "primn",     "secdn"),
                      lu  = c("crop",  "crop",  "crop",  "crop",  "crop",  "past", "range",
                              "primforest", "secdforest", "forestry", "urban", "primother", "secdother"))
    lu <- toolAggregate(luh, map, dim = 3)
    # Attention: mapping maps secdf on both: secdforest and forestry (both contain after aggregation the full secondary
    #           forest area)! Next step will calculate proper shares and multiply it to compute correct areas
    secdf <- c("secdforest", "forestry")
    forestShares <- .expand(.shr(forestArea[, , secdf]), lu)
    lu[, , secdf] <- forestShares * lu[, , secdf]
    return(lu)
  }

  .luTarget <- function(lu, forestArea) {
    forests <- c("primforest", "secdforest", "forestry")
    other   <- c("primother", "secdother")
    nature  <- c(forests, other)

    # Correct for overflow effects (forestArea greater than forest and other land available in luInit)
    overflow <- forestArea[, , "forest"] - dimSums(lu[,,nature], dim = 3)
    overflow[overflow < 0] <- 0
    if (any((of <- dimSums(overflow, dim = 1)) > 0)) {
      vcat(verbosity = 2, paste("Mismatch of FAO forest exceed LUH forest + other land by:", paste0(paste(getYears(of), round(of, 0), "Mha"), collapse = ", "), "- FAO forest data will be cut."))
      # corrected forest areas <- weight of forest subcategories * corrected total forest area
      corr <- setNames((forestArea[, , "forest"] +10e-10 - overflow)/(forestArea[,,"forest"] + 10e-10), NULL)
      forestArea <- corr * forestArea
    }

    # compute other land area (diff between total natural land and forest area)
    otherArea <- setNames(dimSums(lu[,,nature], dim = 3) - forestArea[,,"forest"], NULL)
    if(any(otherArea < -10e-6)) {
      vcat(0, "Computed other land area is partly negative. This should not be the case! values will be corrected to 0.")
    }
    # due to rounding there are always some very small values below 0 which is why it is always corrected to 0, but
    # a warning is only triggered for values smaller than 10e-6
    otherArea[otherArea<0] <- 0

    # Split other area into primary and secondary based on shares in lu
    otherShare <- (lu[,,other] + 10^-10)/dimSums(lu[,,other] + 10^-10, dim = 3)

    # update other and forest areas
    lu[,,other] <- otherShare * otherArea
    lu[,,forests] <- forestArea[,,forests]
    return(lu)
  }

  luh <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE, cellular = TRUE,
                    selectyears = selectyears, cells = cells, aggregate = FALSE)
  forestArea <- calcOutput("ForestArea", selectyears = selectyears, aggregate = FALSE)
  # rename categories and split secondary forest into secondary forest and forestry
  # based on forestArea information (area sizes kept as reported by luh)
  lu <- .luIni(luh, forestArea)
  luCountry <- toolSum2Country(lu)
  luTarget <- .luTarget(luCountry, forestArea)

  vegC  <- calcOutput("LPJmL_new", version = "LPJmL4_for_MAgPIE_44ac93de", climatetype = "GSWP3-W5E5:historical",
                      subtype = "vegc", stage = "smoothed", aggregate = FALSE)[, selectyears, ]
  vegC <- toolCoord2Isocell(vegC, cells = cells)

  out <- toolForestRelocate(lu = lu, luCountry = luCountry, luTarget = luTarget, vegC = vegC)
  if (any(out < 0)) {
    out[out < 0] <- 0
    vcat(0, "Negativ land values detected and replaced by 0.")
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    min = 0,
    max = 14900, ### global land area
    description = "Land use initialisation data for different land pools",
    isocountries = FALSE
  ))
}
