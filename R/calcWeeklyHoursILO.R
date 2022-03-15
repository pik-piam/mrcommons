#' @title calcWeeklyHoursILO
#' @description calculates complete dataset of mean weekly hours worked by people employed in agriculture, forestry and
#' fishery based on ILO dataset
#' @param projections boolean, should weekly hours be projected up to 2100?
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("WeeklyHoursILO")
#' }
#' @importFrom magclass getNames<- getYears getRegions where time_interpolate dimSums

calcWeeklyHoursILO <- function(projections = FALSE) {

  ilo <- readSource("ILOSTAT", "WeeklyHoursByActivity")[, , list("Total", "Aggregate: Agriculture"), drop = TRUE]
  getNames(ilo) <-  "Agriculture, forestry and fishing"
  ilo <- ilo[, where(ilo != 0)$true$years, ]
  ilo[ilo == 0] <- NA

  if (isTRUE(projections)) {
    addYears <- setdiff(paste0("y", seq(1965, 2100, 5)), getItems(ilo, dim = 2))
    ilo <- magpiesort(add_columns(ilo, addnm = addYears, dim = 2, fill = NA))
  }

  # fill gaps with estimates (for countries with at least 10 observations)
  years <- getYears(ilo, as.integer = TRUE)
  minYear <- min(getYears(ilo, as.integer = TRUE))
  maxYear <- max(getYears(ilo, as.integer = TRUE))
  for (reg in getItems(ilo, dim = 1)) {
    tmp <- ilo[reg, , ]
    tmpYears <- as.integer(gsub("y", "", where(is.finite(tmp))$true$years))
    if (length(tmpYears) >= 10) {
      # 1. extrapolate using mean of three closest years
      yPast <- years[years < min(tmpYears)]
      if (length(yPast) > 0) {
        ilo[reg, yPast, ] <-
                    mean(tmp[, min(tmpYears):(min(min(tmpYears) + 2, maxYear)), ], na.rm = TRUE)
      }
      yFuture <- years[years > max(tmpYears)]
      if (length(yFuture > 0)) {
        ilo[reg, yFuture, ] <-
                    mean(tmp[, (max(max(tmpYears) - 2, minYear)):max(tmpYears), ], na.rm = TRUE)
      }
      # 2. fill gaps within time series through linear interpolation
      yGaps <- where(!is.finite(ilo[reg, , ]))$true$years
      if (length(yGaps) > 0) {
        ilo[reg, , ] <- time_interpolate(ilo[reg, , ][, yGaps, , invert = TRUE],
                                         interpolated_year = yGaps,
                                         integrate_interpolated_years = TRUE)
      }
    }
  }

  # agricultural employment as weight (keep weight in missing years constant)
  agEmpl <- calcOutput("AgEmplILO", aggregate = FALSE, subsectors = FALSE)
  agEmpl <- time_interpolate(agEmpl, interpolated_year = setdiff(getItems(ilo, dim = 2), getItems(agEmpl, dim = 2)),
                             integrate_interpolated_years = TRUE, extrapolation_type = "constant")

  # fill countries that are completely missing with world averages (weighted with agricultural employment per country)
  regionsNotComplete <- where(!is.finite(ilo))$true$regions
  weight1 <- agEmpl
  weight1[regionsNotComplete, , ] <- 0
  weight1 <- weight1 / dimSums(weight1, dim = 1)

  regionsFilled <- setdiff(getItems(ilo, dim = 1), regionsNotComplete)
  worldAvg <- dimSums(ilo[regionsFilled, , ] * weight1[regionsFilled, , ], dim = 1)
  ilo[regionsNotComplete, , ] <- worldAvg

  # weight for aggregation to world regions
  weight2 <- agEmpl

  return(list(x = ilo,
              weight = weight2,
              unit = "hours per week",
              description = "Mean weekly hours actually worked per employed person by sex and economic activity"))
}
