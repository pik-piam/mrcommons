#' @title calcPhysicalInactivity
#' @description physical inactivity level estimate based on
#' Hallal, Pedro C, Lars Bo Andersen, Fiona C Bull, Regina Guthold, William Haskell, and Ulf Ekelund. 2012.
#' "Global Physical Activity Levels: Surveillance Progress, Pitfalls, and Prospects."
#' The Lancet 380 (9838):247-57. https://doi.org/10.1016/S0140-6736(12)60646-1.
#' @param update WHO estimates from http://apps.who.int/gho/data/view.main.2487?lang=en seem to have updated.
#' TRUE provides the results as they were online on 27.2.2024
#' @export

calcPhysicalInactivity <- function(update = TRUE) {
  dev <- calcOutput("DevelopmentState", aggregate = FALSE)
  dem <-
    calcOutput("Demography", aggregate = FALSE, education = FALSE)

  inactivity <- dem * 0 + 1

  # set default values for countries without data
  # assumptions based on
  # http://apps.who.int/gho/data/view.main.2487?lang=en
  # http://apps.who.int/gho/data/view.main.2487ADO?lang=en
  underaged <- c("0--4", "5--9", "10--14", "15--19")
  working <-
    c("20--24",
      "25--29",
      "30--34",
      "35--39",
      "40--44",
      "45--49",
      "50--54",
      "55--59")
  retired <-
    c("60--64",
      "65--69",
      "70--74",
      "75--79",
      "80--84",
      "85--89",
      "90--94",
      "95--99")

  if (update == FALSE) {
    inactivity[, , "M"] <- dev * 0.277 + (1 - dev) * 0.107
    inactivity[, , "F"] <- dev * 0.376 + (1 - dev) * 0.224
    inactivity[, , underaged][, , "M"] <- 0.776
    inactivity[, , underaged][, , "F"] <- 0.839
  } else {
    inactivity[, , "M"] <- dev * 0.320 + (1 - dev) * 0.134
    inactivity[, , "F"] <- dev * 0.416 + (1 - dev) * 0.188
    # using values of 2010
    inactivity[, , underaged][, , "M"] <-
      dev * 0.749 + (1 - dev) * 0.851
    inactivity[, , underaged][, , "F"] <-
      dev * 0.855 + (1 - dev) * 0.873
  }

  observations <-
    readSource("WHO", "physical_inactivity_adults") / 100
  observedcountries <- where(!is.na(observations))$true$regions

  # calibrate to observations
  inactivity[observedcountries, , c(working, retired)] <-
    inactivity[observedcountries, , c(working, retired)] +
    setYears(observations[observedcountries, 2010, ] -
               inactivity[observedcountries, 2010, c(working, retired)][, , "SSP2"],
             NULL)

  observations <-
    readSource("WHO", "physical_inactivity_underaged") / 100
  observedcountries <- where(!is.na(observations))$true$regions

  inactivity[observedcountries, , c(underaged)] <-
    inactivity[observedcountries, , c(underaged)] +
    setYears(observations[observedcountries, 2010, ] -
               inactivity[observedcountries, 2010, c(underaged)][, , "SSP2"],
             NULL)

  inactivity[inactivity < 0.03] <- 0.03 # set reasonable bounds
  inactivity[inactivity > 0.97] <- 0.97 # set reasonable bounds

  inactivity2 <- inactivity
  # informed assumption based on Hallal et al: Lower activity among retired adults
  inactivity2[, , working] <-
    inactivity2[, , working] / (1.5 ^ 0.5) # results in the effect that a/b=2/3
  inactivity2[, , retired] <- inactivity2[, , retired] * (1.5 ^ 0.5)


  getSets(inactivity2) <-
    c("region", "year", "scenario", "sex", "age")

  # reduce inactivity levels to 0 in the SDP scenario
  inactivity2[, , "SDP"] <-
    convergence(
      origin = inactivity2[, , "SDP"],
      aim = 0,
      start_year = "y2020",
      end_year = "y2050",
      type = "linear"
    )

  return(
    list(
      x = inactivity2,
      weight = dem,
      unit = "kcal per capita per day",
      description = "Intake estimate based on normalized body weights",
      min = 0,
      max = 1
    )
  )
}
