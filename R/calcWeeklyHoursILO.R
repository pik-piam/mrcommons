#' @title calcWeeklyHoursILO
#' @description calculates complete dataset of mean weekly hours worked by people employed in agriculture, forestry and
#' fishery based on ILO dataset
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("WeeklyHoursILO")
#' }
#' @importFrom magclass getNames<- getYears getRegions where time_interpolate dimSums

calcWeeklyHoursILO <- function() {
  ilo <- readSource("ILOSTAT", "WeeklyHoursByActivity")[, , list("Total", "Aggregate: Agriculture"), drop = TRUE]
  getNames(ilo) <- "Agriculture, forestry and fishing"
  ilo[ilo == 0] <- NA

  # fill gaps with estimates (for countries with at least 10 observations)
  minYear <- min(getYears(ilo, as.integer = TRUE))
  maxYear <- max(getYears(ilo, as.integer = TRUE))
  regs <- c()
  for (reg in getRegions(ilo)) {
    tmp <- ilo[reg, , ]
    tmpYears <- as.integer(gsub("y", "", where(is.finite(tmp))$true$years))
    if (length(tmpYears) >= 10) {
      regs <- c(regs, reg)
      # 1. extrapolate using mean of three closest years
      if (minYear < min(tmpYears)) {
        ilo[reg, minYear:(min(tmpYears) - 1), ] <-
                    mean(tmp[, min(tmpYears):(min(min(tmpYears) + 2, maxYear)), ], na.rm = TRUE)
      }
      if (maxYear > max(tmpYears)) {
        ilo[reg, (max(tmpYears) + 1):maxYear, ] <-
                    mean(tmp[, (max(max(tmpYears) - 2, minYear)):max(tmpYears), ], na.rm = TRUE)
      }
      # 2. fill gaps within time series through linear interpolation
      years <- where(!is.finite(ilo[reg, , ]))$true$years
      if (length(years) > 0) {
        ilo[reg, , ] <- time_interpolate(ilo[reg, , ][, years, , invert = TRUE],
                                         interpolated_year = years,
                                         integrate_interpolated_years = TRUE)
      }
    }
  }

  # agricultural employment as weight -> reduce time series to years that have data
  agEmpl <- calcOutput("AgEmplILO", aggregate = FALSE)
  ilo <- ilo[, getYears(agEmpl), ]

  # fill countries that are completely missing with world averages (weighted with agricultural employment per country)
  regionsNotComplete <- where(!is.finite(ilo))$true$regions
  weight1 <- agEmpl
  weight1[regionsNotComplete, , ] <- 0
  weight1 <- weight1 / dimSums(weight1, dim = 1)

  regionsFilled <- setdiff(getRegions(ilo), regionsNotComplete)
  worldAvg <- dimSums(ilo[regionsFilled, , ] * weight1[regionsFilled, , ], dim = 1)
  ilo[regionsNotComplete, , ] <- worldAvg

  # weight for aggregation to world regions
  weight2 <- agEmpl

  return(list(x = ilo,
              weight = weight2,
              unit = "hours per week",
              description = "Mean weekly hours actually worked per employed person by sex and economic activity"))
}
