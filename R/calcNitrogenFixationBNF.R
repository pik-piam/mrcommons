#' @title calcNitrogenFixationPast
#' @description calculates fixation from nitrogen-fixing crops
#' @param sum_plantparts if false, crop residues, belowground residues and harvested organ are reported separately
#' @param cellular cellular estimates optional
#' @param irrigation if TRUE, distinguishes irrigated and non-irrigated crops
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcNitrogenFixationPast()]
#' @examples
#' \dontrun{
#' calcOutput("calcNitrogenFixationPast")
#' }
#' @importFrom magpiesets findset
#'

calcNitrogenFixationBNF <- function(sum_plantparts = TRUE, # nolint: object_name_linter.
                                    cellular = FALSE,
                                    irrigation = FALSE) {
  fixBiomass <- NULL
  past <- findset("past")

  harvest <- collapseNames(calcOutput("Production", products = "kcr", cellular = cellular,
                                      attributes = "nr", irrigation = irrigation, aggregate = FALSE)[, past, ])
  harvest <- add_dimension(harvest, dim = 3.1, add = "data1", nm = "organ")
  resAg <- collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                    plantparts = "ag", irrigation = irrigation,
                                    attributes = "nr", aggregate = FALSE)[, past, ],
                         collapsedim = "attributes")
  resBg <- collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                    plantparts = "bg", irrigation = irrigation,
                                    attributes = "nr", aggregate = FALSE)[, past, ],
                         collapsedim = "attributes")
  biomass <- mbind(harvest, resAg, resBg)
  if (sum_plantparts) {
    biomass <- dimSums(biomass, dim = 3.1)
  }
  ndfa <- setYears(readSource("Herridge", subtype = "ndfa"), NULL)
  ndfa <- ndfa[getItems(biomass, dim = if (dimExists("iso", biomass)) "iso" else 1.1), , ]
  biomass <- biomass * ndfa
  fixBiomass <- add_dimension(biomass, dim = 3.1, nm = "fixation_crops")


  return(list(x = fixBiomass,
              weight = NULL,
              unit = "Mt Nr",
              description = "Biological nitrogen fixation by crops",
              isocountries = !cellular))
}
