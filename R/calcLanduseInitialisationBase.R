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

  luh <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE, cellular = TRUE,
                    selectyears = selectyears, cells = cells, aggregate = FALSE)
  forestArea <- calcOutput("ForestArea", selectyears = selectyears, aggregate = FALSE)
  lu <- .luIni(luh, forestArea)

  vegC  <- calcOutput("LPJmL_new", version = "LPJmL4_for_MAgPIE_44ac93de", climatetype = "GSWP3-W5E5:historical",
                      subtype = "vegc", stage = "smoothed", aggregate = FALSE)[, selectyears, ]
  vegC <- toolCoord2Isocell(vegC, cells = cells)


  out <- toolFAOForestRelocate(luInit = lu, forestArea = forestArea, vegC = vegC)
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
