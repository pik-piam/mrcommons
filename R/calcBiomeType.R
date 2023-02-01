#' @title calcBiomeType
#'
#' @description Returns fraction of spatial unit (cell) belonging to a biome
#' type of each biogeographic realm. The classification is based on data from
#' 'the nature conservancy'
#' (https://geospatial.tnc.org/datasets/b1636d640ede4d6ca8f5e369f2dc368b/about)
#' originally developed by Olson et al. (2001), BioScience.
#'
#' @param cells magpiecell (59199 cells) or lpjcell (67420 cells)
#'
#' @return List with a magpie object
#' @author Patrick v. Jeetze
#' @seealso
#' \code{\link{readTNC2019}}
#'
#' @examples
#' \dontrun{
#' calcOutput("BiomeType", aggregate = FALSE)
#' }
#'
calcBiomeType <- function(cells = "magpiecell") {
  # get processed data from 'the nature conservancy'
  x <- readSource("TNC2019", convert = "onlycorrect")

  if (cells == "magpiecell") {
    x <- toolCoord2Isocell(x)
  } else if (cells != "lpjcell") {
    stop("Please specify cells argument")
  }

  weight <- calcOutput("LanduseInitialisation",
    aggregate = FALSE, cellular = TRUE,
    input_magpie = TRUE, years = "y1995", round = 6
  )
  weight <- dimSums(weight, dim = 3)

  if (length(unique(dimSums(x, dim = 3))) > 2) {
    stop("Sum over all biome types != 1 or 0. Check readTNC2019 for errors.")
  }
  # do not apply weight where sum over all biome types is zero
  weight <- weight * dimSums(x, dim = 3)

  return(list(
    x = x,
    weight = weight,
    unit = "Share",
    description = "Share of biome type of each biogeographic realm in each spatial unit (cell)",
    isocountries = FALSE
  ))
}
