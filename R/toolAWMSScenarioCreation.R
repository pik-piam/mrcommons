#' @title toolAWMSScenarioCreation
#' @description tool function to calculate the share of manure managed in different animal waste management systems
#'              in confinements.
#' @param name Name of the scenario
#' @param startYear Year were prediction starts
#' @param categories share of manure managed in different animal waste management systems
#' @param values target values
#' @param out contains historical data
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Edna J. Molina Bacca
#' @seealso
#' [calcAWMSconfShr()]
#'

toolAWMSScenarioCreation <- function(name, startYear, categories, values, out) {

  lapply(lapply(values, sum), function(x) {
    if (x != 1) {
      stop("Categories do not sum one")
    }
  })

  yearsList <- names(values)

  for (i in seq_along(values)) {

    targetAim <- unlist(values[i], use.names = FALSE)
    names(targetAim) <- categories
    # These declarations account for the specific targets of each year of the scenario
    yearTarget <- out[, startYear, "constant"] * targetAim["traditional"]
    yearTarget[, , "digester"] <- yearTarget[, , "digester"] + targetAim["digester"]
    yearTarget[, , "daily_spread"] <- yearTarget[, , "daily_spread"] + targetAim["daily_spread"]


    if (i == 1) {
      scenarioX <- convergence(origin = out[, , "constant"],
                               aim = setYears(yearTarget, NULL),
                               start_year = startYear,
                               end_year = yearsList[i],
                               direction = NULL,
                               type = "linear")
    } else {
      scenarioX <- convergence(origin = scenarioX,
                               aim = setYears(yearTarget, NULL),
                               start_year = yearsList[i - 1],
                               end_year = yearsList[i],
                               direction = NULL,
                               type = "linear")
    }
  }
  getNames(scenarioX, dim = 1) <- name
  return(mbind(out, scenarioX))
}
