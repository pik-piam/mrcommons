#' @title calcClimateClass
#' @description fraction of a cell belonging to a given climate classification based on
#'              different climate cliassification schemes
#'
#' @param datasource select source from:
#'                   - koeppen for Koeppen Geiger Classification
#'                     http://koeppen-geiger.vu-wien.ac.at/
#'                   - ipcc, ipccReduced, ipccReduced2019 for IPCC Guideline climate classification
#*                     `https://esdac.jrc.ec.europa.eu/projects/RenewableEnergy/Data/Climate_Zone.zip`
#' @param cells "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#'
#' @return Clustered MAgPIE object on requested resolution
#' @author Abhijeet Mishra, Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("ClimateClass", aggregate = FALSE)
#' }
#'
#' @export

calcClimateClass <- function(datasource = "koeppen", cells = "lpjcell") {

  if (datasource == "koeppen") {

    x      <- readSource("Koeppen", subtype = "cellular", convert = "onlycorrect")

  } else if (grepl("ipcc", datasource)) {

    x <- readSource("IPCCClimate", convert = "onlycorrect")
    getNames(x) <- gsub(" ", "_", tolower(getNames(x)))

    if (grepl("ipccReduced", datasource)) {
      reduceIPCC  <- toolGetMapping("IPCC2IPCCreduced.csv", type = "sectoral")
      x           <- toolAggregate(x, reduceIPCC, from = "ipcc", to = datasource, dim = 3, partrel = TRUE)
    }

  } else {
    stop("Source inc calcClimateClass unkown.")
  }

  if (cells == "magpiecell") x <- toolCoord2Isocell(x)
  weight <- calcOutput("LandArea", cells = cells, aggregate = FALSE)

  return(list(x = x,
              weight = weight,
              unit = "share",
              description = paste("Climate classification according to:", datasource),
              isocountries = FALSE))
}
