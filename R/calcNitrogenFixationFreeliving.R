#' @title calcNitrogenFixationFreeliving
#' @description calculates fixation from freeliving bacteria
#' @param cellular cellular estimates optional
#' @param irrigation if TRUE, distinguishes irrigated and non-irrigated crops
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("NitrogenFixationFreeliving")
#' }
#' @importFrom magpiesets findset
#'

calcNitrogenFixationFreeliving <- function(cellular = FALSE, irrigation = FALSE) {

  area <- collapseNames(calcOutput("Croparea", cellular = cellular, aggregate = FALSE,
                                   physical = TRUE, irrigation = irrigation, fallow = TRUE))

  freeliving <- setYears(readSource("Herridge", subtype = "freeliving", convert = FALSE), NULL)
  freeliving <- mbind(
    freeliving,
    setNames(freeliving[, , "tece"], "fallow") # use value of temperate cereals for fallow.
    # justification: bare fallow has likely a bit less (as less C for Nfixing bacteria available)
    # while green fallow has a bit more, but all not too much different as long as no
    # planted green fallow with legumes is present, which is usually not.
  )
  freeliving <- area * freeliving
  fixFreeliving <- add_dimension(x = freeliving, dim = 3.1,
                                 add = "fixation", nm = "fixation_freeliving")

  return(list(x = fixFreeliving,
              weight = NULL,
              unit = "Mt Nr",
              description = "Nitrogen fixation by freeliving microorganisms",
              isocountries = !cellular))
}
