#' @title calcResBiomass
#' @description Provides MAgPIE-FEED data for aboveground and belowground residues biomass
#'
#' @param cellular   If TRUE calculation and output on cellular level
#' @param cells      Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param plantparts both, ag (aboveground) or belowground (bg). Both can have memory
#'                   problems for cellular outputs
#' @param irrigation if TRUE, distinguishes irrigated and non-irrigated crops
#' @param attributes in dm, wm, ge, nr, p, k
#' @param scenario   define scenario switch for sensititvy analysis
#'                   for historical SOC budget
#'
#' @return MAgPIE-FEED data for ProdResAg and corresonding weights as a list of
#' two MAgPIE objects
#' @author Lavinia Baumstark, Isabelle Weindl, Benjamin Bodirsky
#' @seealso [madrat::calcOutput()], [madrat::readSource()]
#' @examples
#' \dontrun{
#' calcOutput("ResBiomass")
#' }
#'
calcResBiomass <- function(cellular = FALSE, cells = "lpjcell",
                           plantparts = "both",
                           irrigation = FALSE, attributes = "all",
                           scenario = "default") {

  croptypesMAG <- findset("kcr")

  # memory problems for cellular data
  if (plantparts == "both") {
    aboveGroundResidues   <- calcOutput("ResBiomass", cellular = cellular,
                                        cells = "lpjcell",
                                        aggregate = FALSE, plantparts = "ag",
                                        irrigation = irrigation,
                                        attributes = attributes,
                                        scenario = scenario)
    belowGroundResidues   <- calcOutput("ResBiomass", cellular = cellular,
                                        cells = "lpjcell",
                                        aggregate = FALSE, plantparts = "bg",
                                        irrigation = irrigation,
                                        attributes = attributes,
                                        scenario = scenario)
    residueProduction     <- mbind(aboveGroundResidues, belowGroundResidues)

  } else if (plantparts %in% c("ag", "bg")) {
    # read in area harvested
    harvestedArea  <- calcOutput("Croparea", sectoral = "kcr", physical = FALSE,
                                 cellular = cellular,
                                 irrigation = irrigation, aggregate = FALSE)
    # cyears here above
    cropProduction <- collapseNames(calcOutput("Production", products = "kcr", attributes = "dm",
                                               cellular = cellular, cells = "lpjcell",
                                               irrigation = irrigation, aggregate = FALSE))
    cyears         <- intersect(getYears(harvestedArea),
                                getYears(cropProduction))

    cropProduction <- cropProduction[, cyears, ]
    harvestedArea  <- harvestedArea[, cyears, ]

    harvestIndex   <- setYears(readSource("HI"), NULL)[, , croptypesMAG]

    if (grepl("freeze*", scenario)) {
      # select year by scenario name
      freezeYear <- as.integer(gsub("freeze", "", scenario))

      # calculate yields and freeze yield levels
      cropYields  <- toolConditionalReplace(cropProduction[, cyears, ] /
                                              harvestedArea[, cyears, ],
                                            c("is.na()", "is.infinite()"), 0)
      cropYields  <- toolFreezeEffect(cropYields, freezeYear,
                                      constrain = "first_use")

      # recalculate production
      cropProduction <- cropYields[, cyears, ] * harvestedArea[, cyears, ]
    }

    if (plantparts == "ag") {
      # calculate residue production
      resWithProduction     <- cropProduction * collapseNames(harvestIndex[, , "slope"])
      resWithHarvestedArea  <- harvestedArea  * collapseNames(harvestIndex[, , "intercept"])

      aboveGroundResidues   <- (resWithProduction[, cyears, ] +
                                  resWithHarvestedArea[, cyears])
      # read in residues attributes
      attributesAboveGround <- readSource("ProductAttributes",
                                          subtype = "AgResidues")[, , croptypesMAG]
      if (!all(attributes %in% "all")) { # for problems with memory size
        attributesAboveGround <- attributesAboveGround[, , attributes]
      }
      aboveGroundResidues   <- aboveGroundResidues * attributesAboveGround
      residueProduction     <- add_dimension(aboveGroundResidues, dim = 3.1,
                                             add = "residues", nm = "ag")

    } else if (plantparts == "bg") {

      aboveGroundResidues <- collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                                      cells = "lpjcell",
                                                      plantparts = "ag", attributes = "dm",
                                                      irrigation = irrigation, aggregate = FALSE,
                                                      scenario = scenario))
      # read harvest index
      belowGroundResidues <- (aboveGroundResidues + cropProduction) *
        collapseNames(harvestIndex[, , "bg_to_ag"])
      # read in residues attributes
      attributesBelowGround <- readSource("ProductAttributes",
                                          subtype = "BgResidues")[, , croptypesMAG]

      if (!all(attributes %in% getNames(attributesBelowGround, dim = 1))) {
        # for problems with memory size
        residueProduction   <- belowGroundResidues *  attributesBelowGround
        # add all product attribute to below ground residues (p, k, ge, wm with 0)
        attributesBGResNames  <- getNames(attributesBelowGround, dim = 1)
        attributesNames       <- findset("attributes")
        attributesMissNames   <- setdiff(attributesNames, attributesBGResNames)
        attributesBelowGround <- add_columns(attributesBelowGround,
                                             addnm = attributesMissNames, dim = 3.1)
        attributesBelowGround[, , attributesMissNames] <- 0
      }
      if (!all(attributes %in% "all")) {
        attributesBelowGround <- attributesBelowGround[, , attributes]
      }

      residueProduction   <- belowGroundResidues *  attributesBelowGround
      residueProduction   <- add_dimension(residueProduction, dim = 3.1,
                                           add = "residues", nm = "bg")
    }
  } else {
    stop("unkown plantpart")
  }

  if (!all(attributes %in% "all")) { # for problems with memory size
    residueProduction <- residueProduction[, , attributes]
  }

  if (cellular) {
    if (cells == "magpiecell") {
      residueProduction <- toolCoord2Isocell(residueProduction, cells = cells)
    }
  }

  return(list(x            = residueProduction,
              weight       = NULL,
              unit         = "mio ton DM,Nr,P,K,WM",
              description  = "aboveground and belowground residues production",
              isocountries = !cellular)
  )
}
