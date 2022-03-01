#' @title calcProtectArea
#' @description Function extracts conservation protected area
#'
#' @param cells number of cells of landmask (select "magpiecell" for 59199 cells or "lpjcell" for 67420 cells)
#' @param bhifl should be TRUE (including BH_IFL scenario)
#'              for cellular preprocessing revisions > 4.65
#'
#' @return magpie object in cellular resolution with different protection scenarios
#' @author Felicitas Beier, David Chen
#'
#' @examples
#' \dontrun{
#' calcOutput("ProtectArea", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset addLocation
#' @importFrom magclass collapseDim
#'

calcProtectArea <- function(cells = "magpiecell", bhifl = TRUE) {

  # Land area (in Mha):
  landArea <- calcOutput("LanduseInitialisation", cellular = TRUE, cells = cells,
                         nclasses = "seven", fao_corr = TRUE, input_magpie = TRUE,
                         years = "y1995", aggregate = FALSE)
  landArea <- dimSums(landArea, dim = 3)

  # Protection Area mz file (conservation priority area in Mha)
  x <- readSource("ProtectArea", convert = "onlycorrect")

  if (bhifl) {
    # In this data set, FF stands for intact forest landscapes (IFL)
    getNames(x) <- gsub("FF", "IFL", getNames(x))

    # Add BH_IFL scenario (combination of biodiversity hotspots and intact forestry landscapes)
    # (Note: should only be applied to forests (for other land, use BH))
    bhifl <- setNames(pmax(x[, , "BH"], x[, , "IFL"]), nm = "BH_IFL")
    x     <- mbind(x, bhifl)
  }

  # Half Earth Protection Share
  protectShr           <- readSource("HalfEarth", convert = "onlycorrect")
  getNames(protectShr) <- "HalfEarth"
  getSets(protectShr)  <- c("x", "y", "iso", "year",  "data")

  if (cells == "magpiecell") {

    protectShr <- toolCoord2Isocell(protectShr, cells = cells)

  } else if (cells == "lpjcell") {

    landArea <- collapseDim(addLocation(landArea), dim = c("N", "region"))

    tmp <- collapseDim(addLocation(x), dim = c("region", "cell"))

    x   <- new.magpie(cells_and_regions = getCells(collapseDim(protectShr, dim = "iso")),
                      years = getYears(tmp),
                      names = getNames(tmp), fill = 0,
                      sets = c("x.y.iso", "year", "data"))
    x[getCells(tmp), , ] <- tmp

    map         <- toolGetMappingCoord2Country()
    if (any(getCells(x) != map$coords)) {
      stop("Wrong cell ordering in calcProtectArea")
    }
    getCells(x) <- paste(map$coords, map$iso, sep = ".")

  } else {
    stop("Please select magpiecell or lpjcell in cells argument of calcProtectArea")
  }

  # Land area to be protected by 2050 (in Mha)
  protectArea <- protectShr * landArea

  # Add HalfEarth scenario to Protection Area file
  x <- mbind(x, protectArea)

  return(list(x            = x,
              weight       = NULL,
              unit         = "Mha",
              description  = "conservation priority areas",
              isocountries = FALSE))
}
