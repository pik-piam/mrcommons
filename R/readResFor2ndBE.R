#' @title readResFor2ndBE
#' @description Read in old ReMIND use of residues for 2nd generation bioenergy and newly estimations
#'
#' @return List of magpie objects with results on old ReMIND regions level
#' @param subtype oldReMIND, newAgriSupply
#' @author Kristine Karstens
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("ResFor2ndBE", subtype = "oldReMIND")
#' }
#'
readResFor2ndBE <- function(subtype) {

  if (subtype == "oldReMIND") {

    residues_remind_EJ <- NULL        #nolint
    load("residues_remind_EJ.Rdata")
    forestShare  <- 42.31301 / 100
    agriShare    <- 1 - forestShare

    yearsIntersect <- intersect(findset("time"), getYears(residues_remind_EJ))

    ### unit conversion from EJ in PJ, use time subset
    residuesRemind <- residues_remind_EJ[, yearsIntersect, ] * 10^3

    ### divide into residues from agriculture and forestry
    out <- mbind(setNames(residuesRemind * forestShare, "res_wood"),
                 setNames(residuesRemind * agriShare,   "res_crop"))

    getSets(out) <- c("old_reg", "t", "residues")

  } else if (subtype == "newAgriSupply") {

    out <- read.magpie("ResAvailFor2ndBE.cs3")
    getSets(out) <- c("iso", "t", "kres", "scenario")
  }

  return(out)
}
