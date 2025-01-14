#' @title calc1stBioDem
#' @description
#' Calculates projections of first generation biofuels demand,including biogas, bioethamol and
#' biodiesel, from IEA database. The unit is Petajoule.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @param subtype all per default. ethanol_oils for selecting 1st gen crop types relevant for REMIND input.
#' @author Xiaoxi Wang, David Klein
#' @seealso
#' [calc1stBioenergyPast()]
#' @examples
#' \dontrun{
#' calcOutput("1stBioDem")
#' }
#' @importFrom magpiesets findset

calc1stBioDem <- function(subtype = "all") {
  past <- findset("past")
  time <- findset("time")

  x <- collapseNames(calcOutput(type = "FAOmassbalance", aggregate = FALSE)[, , "bioenergy"][, , "ge"])
  x <- add_columns(x, dim = 2.1, addnm = setdiff(time, past))
  x[, setdiff(time, past), ] <- 0

  # for oil and ethanol replace FAO with Lotze-Campen projection 
  bioenergyProjection <- readSource("LotzeCampenBiofuel") [, , c("oils", "ethanol")]
  x[, c("y2020", "y2030"), c("oils", "ethanol")] <- bioenergyProjection[, c("y2020", "y2030"), c("oils", "ethanol")]
  
  # if values decline, keep them constant
  x <- x[, sort(getYears(x)), ]
  for (y in 2:length(getYears(x))) {
      x[, y, c("oils", "ethanol")] <- pmax(x[, y, c("oils", "ethanol")],
                                           setYears(x[, y - 1, c("oils", "ethanol")], NULL))
  }
  
  # interpolate years
  x[, , c("oils", "ethanol")] <-
    time_interpolate(dataset = x[, , c("oils", "ethanol")][, c("y2015", "y2025"), , invert = TRUE],
                     interpolated_year = c("y2015", "y2025"),
                     integrate_interpolated_years = TRUE)
  
  # if countries have no oil data in 2020 ...
  countriesWithoutValuesOils <- where(x[, "y2020", "oils"] == 0)$true$regions
  # ... fill 2015-2030 with 2010 values
  x[countriesWithoutValuesOils, c("y2015", "y2020", "y2025", "y2030"), "oils"] <-
    setYears(x[countriesWithoutValuesOils, c("y2010"), "oils"], NULL)
  x[countriesWithoutValuesOils, c("y2015", "y2020", "y2025", "y2030"), "ethanol"] <-
    setYears(x[countriesWithoutValuesOils, c("y2010"), "ethanol"], NULL)

  ### create scenarios beyond 2030 for oil and ethanol
  x <- add_dimension(x = x, dim = 3.1, add = "scenario", nm = "const2030")
  x <- add_columns(x = x, addnm = "const2020", dim = 3.1)
  x <- add_columns(x = x, addnm = "phaseout2020", dim = 3.1)
  x[, , ] <- x[, , "const2030"]

  # scenario "const2030": keep values constant after 2030
  yearstmp <- paste0("y", seq(2030, 2150, 5))
  x[, yearstmp, "oils"][, , "const2030"] <- setYears(x[, c("y2030"), "oils"][, , "const2030"], NULL)
  x[, yearstmp, "ethanol"][, , "const2030"]  <- setYears(x[, c("y2030"), "ethanol"][, , "const2030"], NULL)

  # scenario "const2020": keep values constant after 2020
  yearstmp <- paste0("y", seq(2025, 2150, 5))
  x[, yearstmp, "oils"][, , "const2020"] <- setYears(x[, c("y2020"), "oils"][, , "const2030"], NULL)
  x[, yearstmp, "ethanol"][, , "const2020"]  <- setYears(x[, c("y2020"), "ethanol"][, , "const2030"], NULL)

  # scenario "phaseout2020": phase out to zero between 2020 and 2040
  b <- convergence(origin = x[, , c("oils", "ethanol")][, , "const2020"],
                   aim = 0, start_year = "y2020", end_year = "y2040", type = "linear")
  x[, getYears(b), c("oils", "ethanol")][, , "phaseout2020"] <- b

  ### phase out of crop residues
  a <- x[, , c("res_cereals", "res_nonfibrous", "res_fibrous")]
  a[, getYears(a, as.integer = TRUE) > 2010, ] <- setYears(a[, 2010, ], NULL)
  w <- convergence(a, aim = 0, start_year = 2010, end_year = 2060, type = "s")
  x[, getYears(w), c("res_cereals", "res_nonfibrous", "res_fibrous")] <- w

  x <- clean_magpie(x)

  if (subtype == "ethanol_oils") {
    x <- x[, , c("ethanol", "oils")][, , "const2030"]
    x <- collapseNames(x)
    # rename magpie crop names to remind crop names
    getNames(x) <- gsub("ethanol", "pebios", getNames(x))
    getNames(x) <- gsub("oils", "pebioil", getNames(x))
  }

  return(list(x = x, weight = NULL,
              unit = "PJ",
              description = "1st generation bioenergy demand for different scenarios based on data from FAO and Lotze-Campen 2014") #nolint
  )
}
