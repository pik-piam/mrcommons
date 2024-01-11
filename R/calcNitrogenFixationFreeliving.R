#' @title calcNitrogenFixationPast
#' @description calculates fixation from freeliving bacteria and from nitrogen-fixing crops
#' @param cellular cellular estimates optional
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

calcNitrogenFixationFreeliving <- function(cellular = FALSE) {

  past <- findset("past")

  area <- collapseNames(calcOutput("Croparea", cellular = cellular, aggregate = FALSE,
                                   sectoral = "kcr", physical = TRUE, irrigation = FALSE)[, past, ])
  area <- add_columns(x = area, addnm = "fallow", dim = 3.1)
  area[, , "fallow"] <- calcOutput("FallowLand", cellular = cellular, aggregate = FALSE)[, past, ]

  rates <- setYears(readSource("Herridge", subtype = "freeliving", convert = FALSE), NULL)
  rates <- add_columns(x = rates, addnm = "fallow", dim = 3.1)
  rates[, , "fallow"] <- 0.005 # same as for other crops

  freeliving <- area * rates
  freeliving <- add_dimension(freeliving, dim = 3.1, nm = "fixation_freeliving")

  return(list(x = freeliving,
              weight = NULL,
              unit = "Mt Nr",
              description = "Nitrogen fixation by freeliving bacteria",
              isocountries = !cellular))
}
