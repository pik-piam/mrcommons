#' @title calcAWMSconfShr
#' @description calculates the share of manure managed in different animal waste management systems in confinements.
#' Starting with IPCC 2005 values, turning into scenarios for the future.
#' @param rev revision number of madrat run
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky. Modifications by Edna J. Molina Bacca
#' @seealso
#' [calcAWMSconfShrPast()]
#' @examples
#' \dontrun{
#' calcOutput("AWMSconfShr")
#' }
#' @importFrom magpiesets findset
calcAWMSconfShr <- function(rev = 0.1) {
  past <- findset("past")
  out <- calcOutput("AWMSconfShrPast", aggregate = FALSE)
  weight2 <- collapseNames(calcOutput("Excretion", aggregate = FALSE)[, past, "confinement"][, , "nr"])

  out <- add_dimension(toolHoldConstantBeyondEnd(out), dim = 3.1, nm = "constant")
  weight2 <- toolHoldConstantBeyondEnd(weight2)

  # The order of the  digester and daily_spread and traditional parameters correspond to the years_aim order
  # 2020 target values are global averages of each category for year 2010

  out <- toolAWMSScenarioCreation(name = "ssp1",
                                  startYear = "y2010",
                                  categories = c("digester", "daily_spread", "traditional"),
                                  values = list(y2020 = c(0.04, 0.2, 0.76),
                                                y2050 = c(0.4, 0.2, 0.4),
                                                y2100 = c(0.7, 0.2, 0.1)), out)


  out <- toolAWMSScenarioCreation(name = "ssp2",
                                  startYear = "y2010",
                                  categories = c("digester", "daily_spread", "traditional"),
                                  values = list(y2020 = c(0.04, 0.2, 0.76),
                                                y2050 = c(0.3, 0, 0.7),
                                                y2100 = c(0.6, 0, 0.4)), out)

  out <- toolAWMSScenarioCreation(name = "ssp3",
                                  startYear = "y2010",
                                  categories = c("digester", "daily_spread", "traditional"),
                                  values = list(y2020 = c(0.04, 0.2, 0.76),
                                                y2050 = c(0.2, 0.0, 0.8),
                                                y2100 = c(0.4, 0.0, 0.6)), out)

  out <- toolAWMSScenarioCreation(name = "ssp4",
                                  startYear = "y2010",
                                  categories = c("digester", "daily_spread", "traditional"),
                                  values = list(y2020 = c(0.04, 0.2, 0.76),
                                                y2050 = c(0.4, 0.0, 0.6),
                                                y2100 = c(0.7, 0.0, 0.3)), out)

  out <- toolAWMSScenarioCreation(name = "ssp5",
                                  startYear = "y2010",
                                  categories = c("digester", "daily_spread", "traditional"),
                                  values = list(y2020 = c(0.04, 0.2, 0.76),
                                                y2050 = c(0.4, 0.2, 0.4),
                                                y2100 = c(0.7, 0.2, 0.1)), out)

  out <- toolAWMSScenarioCreation(name = "a1",
                                  startYear = "y2010",
                                  categories = c("digester", "daily_spread", "traditional"),
                                  values = list(y2020 = c(0.04, 0.2, 0.76),
                                                y2050 = c(0.7, 0.0, 0.3),
                                                y2100 = c(0.1, 0.0, 0.9)), out)

  out <- toolAWMSScenarioCreation(name = "a2",
                                  startYear = "y2010",
                                  categories = c("digester", "daily_spread", "traditional"),
                                  values = list(y2020 = c(0.04, 0.2, 0.76),
                                                y2050 = c(0.2, 0.0, 0.8),
                                                y2100 = c(0.5, 0.0, 0.5)), out)

  out <- toolAWMSScenarioCreation(name = "b1",
                                  startYear = "y2010",
                                  categories = c("digester", "daily_spread", "traditional"),
                                  values = list(y2020 = c(0.04, 0.2, 0.76),
                                                y2050 = c(0.3, 0.0, 0.7),
                                                y2100 = c(0.4, 0.0, 0.6)), out)

  out <- toolAWMSScenarioCreation(name = "b2",
                                  startYear = "y2010",
                                  categories = c("digester", "daily_spread", "traditional"),
                                  values = list(y2020 = c(0.04, 0.2, 0.76),
                                                y2050 = c(0.2, 0.0, 0.8),
                                                y2100 = c(0.5, 0.0, 0.5)), out)

  if (rev >= numeric_version("4.33")) {
    out <- toolAWMSScenarioCreation(name = "GoodPractice",
                                    startYear = "y2010",
                                    categories = c("digester", "daily_spread", "traditional"),
                                    values = list(y2015 = c(0.15, 0, 0.85),
                                                  y2030 = c(0.3, 0, 0.7),
                                                  y2050 = c(0.5, 0.0, 0.5),
                                                  y2100 = c(0.7, 0.0, 0.3)), out)
  }

  return(list(x = out,
              weight = weight2,
              unit = "share",
              description = paste("share of excreted nitrogen within stables excreted in",
                                  "which animal waste management systems"),
              min = 0,
              max = 1))
}
