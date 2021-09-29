#' calcPopulationPast
#'
#' Calculates a time series of Population. Different sources are available and
#' can be selected in the madrat config(getConfig()$calc$PopulationPast):
#' \itemize{
#'   \item \code{WDI}: Source: Worldbank. Taiwan was estimated as the
#'         difference between all countries and the global total.
#'   \item \code{UN_PopDiv}: UN Population Division data. Taiwan is estimated
#'         from "other, non-specified areas". Missing countries have their
#'         values set to zero.
#' }
#'
#' @param PopulationPast population past data source
#' @return Population in millions.
#' @author Lavinia Baumstark, Benjamin Bodirsky, Johannes Koch

calcPopulationPast <- function(PopulationPast = "WDI_completed") { # nolint
  populationPast <- PopulationPast

  # Check input argument
  validInputs <- c(
    "WDI",
    "WDI_completed",
    "UN_PopDiv",
    "Eurostat_WDI",
    "Eurostat_WDI_completed"
  )
  if (!populationPast %in% validInputs) {
    stop("Bad input for PopulationPast. Invalid 'PopulationPast' argument.")
  }

  # Look for "_completed" tag. Remove if found.
  if (grepl("_completed$", populationPast)) {
    populationPast <- gsub("_completed", "", populationPast)
    complete <- TRUE
  } else {
    complete <- FALSE
  }

  data <- switch(populationPast,
                 "WDI" = calcPopulationPastWDI(complete),
                 "UN_PopDiv" = readSource("UN_PopDiv"),
                 "Eurostat_WDI" = calcPopulationPastEurostatWDI(complete))

  getNames(data) <- "population"
  data <- clean_magpie(data)

  return(list(x = data,
              weight = NULL,
              unit = "million",
              description = paste0("Population data based on ", populationPast)))
}



######################################################################################
# Functions
######################################################################################
calcPopulationPastWDI <- function(complete) {
  data <- readSource("WDI", "SP.POP.TOTL")

  if (complete) {
    fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
    data <- completeData(data, fill)
  }

  data
}

calcPopulationPastEurostatWDI <- function(complete) {
  dataEurostat <- readSource("Eurostat", "population") / 1e+6
  dataWdi <- readSource("WDI", "SP.POP.TOTL")

  # Get EUR countries.
  eurCountries <- toolGetMapping("regionmappingH12.csv") %>%
    tibble::as_tibble() %>%
    filter(.data$RegionCode == "EUR") %>%
    dplyr::pull(.data$CountryCode)

  # Fill in missing ( == 0) eurostat data using wdi growth rates
  for (c in eurCountries) {
    if (any(dataEurostat[c, , ] == 0) && !all(dataEurostat[c, , ] == 0)) {
      dataEurostat[c, , ] <- harmonizeFutureGrPast(
        past = dataWdi[c, , ],
        future = dataEurostat[c, dataEurostat[c, , ] != 0, ]
      )
    }
  }


  # Use WDI data for everything but the eurCountries. Use Eurostat stat for those.
  data <- dataWdi
  data[eurCountries, , ] <- dataEurostat[eurCountries, , ]

  if (complete) {
    fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
    data <- completeData(data, fill)
  }

  data
}
