#' @title calcLanduseInitialisation
#' @description Calculates the cellular MAgPIE landuse initialisation area.
#'              Data from FAO on forestry is used to split the secondary forest pool
#'              of the LU2v2 dataset into forestry and secd_forest.
#'
#' @param cellular cellular (TRUE) or country-level/regional (FALSE) data?
#'                 For country-level vs regional data: remember to set "aggregate" to FALSE.
#' @param nclasses options are either "six", "seven" or "nine".
#' \itemize{
#' \item "six" includes the original land use classes "crop", "past", "forestry", "forest", "urban" and "other"
#' \item "seven" separates primary and secondary forest and includes "crop", "past", "forestry", "primforest",
#' "secdforest", "urban" and "other"
#' \item "nine" adds the separation of pasture and rangelands, as well as a differentiation of primary
#' and secondary non-forest vegetation and therefore returns "crop", "past", "range", "forestry", "primforest",
#' "secdforest", "urban", "primother" and "secdother"
#' }
#' @param cells        if cellular is TRUE: "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @param input_magpie applies area fix (set cells with zero area to minimal value to
#'                     not disturb aggregating to clusters)
#' @param selectyears  default on "past"
#' @return List of magpie object with results on country or cellular level, weight on cellular level,
#' unit and description.
#' @author Jan Philipp Dietrich, Benjamin Leon Bodirsky, Kristine Karstens, Felcitas Beier, Patrick v. Jeetze
#' @examples
#' \dontrun{
#' calcOutput("LanduseInitialisation")
#' }
#' @importFrom magclass setNames where


calcLanduseInitialisation <- function(cellular = FALSE, nclasses = "seven",
                                      cells = "lpjcell", selectyears = "past",
                                      input_magpie = FALSE) { # nolint

  if (isFALSE(cellular)) {
    out <- calcOutput("LanduseInitialisationBase", cells = "lpjcell",
                       selectyears = selectyears, aggregate = FALSE)
    out <- toolCountryFill(dimSums(out,
                                  dim = c("x", "y")),
                          fill = 0, verbosity = 2)
  } else {
    out <- calcOutput("LanduseInitialisationBase", cells = cells,
                      selectyears = selectyears, aggregate = FALSE)
  }

  if (isTRUE(input_magpie)) {
    # add some small area to completely empty cells to avoid
    # problems in the further processing
    out      <- round(out, 8)
    cellArea <- dimSums(out, dim = 3)
    out[, , "secdother"][cellArea == 0] <- 10^-6
  }

  if (nclasses != "nine") {
    map <- data.frame(nine  = c("crop", "past", "range", "forestry", "primforest", "secdforest",
                                "urban", "primother", "secdother"),
                      seven = c("crop", "past", "past", "forestry", "primforest", "secdforest",
                                "urban", "other", "other"),
                      six   = c("crop", "past", "past", "forestry", "forest", "forest",
                                "urban", "other", "other"))
    if (!(nclasses %in% names(map))) stop("unknown nclasses setting \"", nclasses, "\"")
    out <- toolAggregate(out, rel = map, dim = 3, from = "nine", to = nclasses)
  }

  return(list(x = out,
              weight = NULL,
              unit = "Mha",
              min = 0,
              max = 14900, ### global land area
              description = "Land use initialisation data for different land pools",
              isocountries = !cellular))
}
