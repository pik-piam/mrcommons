#' @title convertResFor2ndBE
#' @description Convert old ReMIND use of residues for 2nd generation
#'              bioenergy to country level data
#'
#' @return List of magpie objects with results on country level
#' @param x MAgPIE object containing original values
#' @param subtype oldReMIND, newAgriSupply
#' @author Kristine Karstens
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("ResFor2ndBE", subtype = "oldReMIND", convert = TRUE)
#' }
#'
convertResFor2ndBE <- function(x, subtype = subtype) {

  if (subtype == "oldReMIND") {

    ### start from global numbers (since old REMIND region
    ### shares are not very sophisticated)
    x <- dimSums(x, dim = 1)
    getItems(x, dim = 1) <- "GLO"

    ### disaggregate residue to county level using forestry areas and
    ### agricultural production of y1995
    landuse <- calcOutput("LanduseInitialisation", nclasses = "seven",
                          aggregate = FALSE)[, "y1995", c("forestry", "crop")]
    glo2iso <- as.data.frame(list(rep("GLO", length(getCells(landuse))),
                                  getCells(landuse)))
    names(glo2iso) <- c("GLO", "iso")

    resWood <- toolAggregate(x[, , "res_wood"], rel = glo2iso,
                             weight = landuse[, , "forestry"],
                             from = "GLO", to = "iso")
    resCrop <- toolAggregate(x[, , "res_crop"], rel = glo2iso,
                             weight = landuse[, , "crop"],
                             from = "GLO", to = "iso")
    out <- toolCountryFill(mbind(resCrop, resWood), fill = 0, verbosity = 0)

  } else if (subtype == "newAgriSupply") {

    out <- toolCountryFill(x, fill = 0, verbosity = 0)
  }

  return(out)
}
