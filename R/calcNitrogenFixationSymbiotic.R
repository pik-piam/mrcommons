#' @title calcNitrogenFixationSymbiotic
#' @description calculates fixation from nitrogen-fixing crops
#' @param cellular cellular estimates optional
#' @param irrigation if TRUE, distinguishes irrigated and non-irrigated crops
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("NitrogenFixationSymbiotic")
#' }
#' @importFrom magpiesets findset
#'

calcNitrogenFixationSymbiotic <- function(cellular = FALSE, irrigation = FALSE) {

  harvest <- collapseNames(calcOutput("Production", products = "kcr", cellular = cellular,
                                      attributes = "nr", irrigation = irrigation, aggregate = FALSE))
  harvest <- add_dimension(harvest, dim = 3.1, add = "data1", nm = "organ")
  resAg <- collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                    plantparts = "ag", irrigation = irrigation,
                                    attributes = "nr", aggregate = FALSE),
                         collapsedim = "attributes")
  resBg <- collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                    plantparts = "bg", irrigation = irrigation,
                                    attributes = "nr", aggregate = FALSE),
                         collapsedim = "attributes")

  commonYears <- intersect(intersect(getYears(resAg), getYears(resBg)), getYears(harvest))
  harvest <- harvest[, commonYears, ]
  resAg   <- resAg[, commonYears, ]
  resBg   <- resBg[, commonYears, ]

  biomass <- mbind(harvest, resAg, resBg)

  ndfa <- setYears(readSource("Herridge", subtype = "ndfa"), NULL)
  ndfa <- ndfa[getItems(biomass, dim = if (dimExists("iso", biomass)) "iso" else 1.1), , ]
  biomass <- biomass * ndfa
  fixBiomass <- add_dimension(biomass, dim = 3.1, add = "fixation", nm = "fixation_crops")

  return(list(x = fixBiomass,
              weight = NULL,
              unit = "Mt Nr",
              description = "Nitrogen fixation by crops",
              isocountries = !cellular))
}
