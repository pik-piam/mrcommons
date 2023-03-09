#' @title calcHourlyLaborCosts
#' @description calculates dataset of hourly labor costs per employee in agriculture
#' @param datasource either raw data from "ILO" (agriculture+forestry+fishery) or data calculated based on total labor
#' costs from "USDA_FAO" (crop+livestock production)
#' @param sector should average hourly labor costs be reported ("agriculture"), or hourly labor costs specific to
#' either "crops" or "livestock" production. For ILO only the aggregate hourly labor costs are available.
#' @param fillWithRegression boolean: should missing values be filled based on a regression between ILO hourly labor
#' costs and GDPpcMER (calibrated to countries)
#' @param calibYear in case of fillWithRegression being TRUE, data after this year will be ignored and calculated using
#' the regression (calibrated for each year to calibYear, or the most recent year with data before calibYear)
#' @param projection either FALSE or SSP on which projections should be based. Only relevant if fillWithRegression is
#' TRUE.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("HourlyLaborCosts")
#' }
#' @importFrom stringr str_split str_to_title


calcHourlyLaborCosts <- function(datasource = "USDA_FAO", sector = "agriculture",  fillWithRegression = TRUE,
                                 calibYear = 2010, projection = FALSE) {

  if (datasource == "ILO" && sector != "agriculture") {
    stop("For ILO only average hourly labor costs in agriculture are available")
  }

  if (isFALSE(fillWithRegression)) {
    if (datasource == "ILO") { # data as reported by ILO (and CACP for India)
      items <- c("ISIC_Rev31: A Agriculture, hunting and forestry", "ISIC_Rev31: B Fishing",
                 "ISIC_Rev4: A Agriculture; forestry and fishing")
      hourlyCosts <- readSource("ILOSTAT", "HourlyLaborCostsByActivity")[, , items]
      hourlyCosts <- hourlyCosts[, , "US$MER2005", drop = TRUE]

      # aggregate within rev 3.1
      mapping <- data.frame(from = c("ISIC_Rev31: A Agriculture, hunting and forestry",
                                     "ISIC_Rev31: B Fishing",
                                     "ISIC_Rev4: A Agriculture; forestry and fishing"),
                            to = c("Agriculture, forestry and fishing",
                                   "Agriculture, forestry and fishing",
                                   "ISIC_Rev4: A Agriculture; forestry and fishing"))
      weight <- hourlyCosts
      weight[weight != 0] <- 1

      hourlyCosts <- toolAggregate(hourlyCosts, rel = mapping, weight = weight, from = "from", to = "to", dim = 3.1)

      # aggregate the two revisions
      mapping <- data.frame(from = c("Agriculture, forestry and fishing",
                                     "ISIC_Rev4: A Agriculture; forestry and fishing"),
                            to = c("Agriculture, forestry and fishing",
                                   "Agriculture, forestry and fishing"))
      weight <- hourlyCosts
      weight[weight != 0] <- 1
      hourlyCosts <- toolAggregate(hourlyCosts, rel = mapping, weight = weight, from = "from", to = "to", dim = 3.1)

      # add data for India for 2000-2017 (except 2006), based on data from the Commission of Agricultural Costs and
      # Prices (https://cacp.dacnet.nic.in/)
      hourlyCostsIndia <- c(0.2063, 0.2080, 0.2015, 0.1970, 0.1936, 0.1964, 0.2014, 0.2350, 0.2520,
                            0.2672, 0.3097, 0.3336, 0.3568, 0.3795, 0.3903, 0.3956, 0.4008)
      hourlyCosts["IND", setdiff(2000:2017, 2006), ] <- hourlyCostsIndia

      # add data for China, aggregated using production as weight (provided by Xiaoxi)
      hourlyCostsChina <- readSource("HourlyLaborCostsChina", convert = FALSE)
      prodkcr <- collapseDim(calcOutput("Production", products = "kcr", attributes = "dm", aggregate = FALSE))
      prodkli <- collapseDim(calcOutput("Production", products = "kli", attributes = "dm", aggregate = FALSE))
      prod <- mbind(prodkcr["CHN", , intersect(getNames(prodkcr), getNames(hourlyCostsChina))],
                    prodkli["CHN", , intersect(getNames(prodkli), getNames(hourlyCostsChina))])
      hourlyCostsChina <- hourlyCostsChina[, , getNames(prod)]
      prod <- time_interpolate(prod, interpolated_year = setdiff(getYears(hourlyCostsChina), getYears(prod)),
                               integrate_interpolated_years = TRUE, extrapolation_type = "constant")
      weight <- (prod / dimSums(prod, dim = 3))[, getYears(hourlyCostsChina), ]
      hourlyCostsChina <- dimSums(hourlyCostsChina * weight, dim = 3)
      hourlyCostsChina[!is.finite(hourlyCostsChina)] <- 0

      years <- intersect(getYears(hourlyCosts), getYears(hourlyCostsChina))
      hourlyCosts["CHN", years, ] <- hourlyCostsChina[, years, ]

      # remove outliers
      hourlyCosts[hourlyCosts > 100] <- 0
      hourlyCosts[c("HRV", "MUS"), , ] <- 0  # unreasonable low values

    } else if (datasource == "USDA_FAO") { # from USDA/FAO labor costs for crop+livst, ag. empl. and weekly hours
      # ag. empl. from ILO
      agEmpl <- calcOutput("AgEmplILO", subsectors = TRUE, aggregate = FALSE)[, , c("Livestock", "Crops")]

      # total labor costs (calculated as VoP * labor cost share)
      totalLaborCosts <- calcOutput("LaborCosts", datasource = "USDA", addSubsidies = TRUE, inclFish = FALSE,
                                    aggregate = FALSE)

      # average weekly hours worked per week
      weeklyHours <- calcOutput("WeeklyHoursILO", aggregate = FALSE)

      # subset to joint years
      years <- intersect(intersect(getItems(totalLaborCosts, dim = 2),
                                   getItems(agEmpl, dim = 2)),
                         getItems(weeklyHours, dim = 2))
      agEmpl <- agEmpl[, years, ]
      totalLaborCosts <- totalLaborCosts[, years, ]
      weeklyHours <- weeklyHours[, years, ]

      # only use employment data where also VoP data is available
      agEmpl[totalLaborCosts == 0] <- 0

      # which sector?
      if (sector != "agriculture") {
        agEmpl <- agEmpl[, , stringr::str_to_title(sector)]
        totalLaborCosts <- totalLaborCosts[, , stringr::str_to_title(sector)]
      } else {
        agEmpl <- dimSums(agEmpl, dim = 3)
        totalLaborCosts <- dimSums(totalLaborCosts, dim = 3)
      }

      # calculate hourly labor costs
      hourlyCosts <- setNames((totalLaborCosts / agEmpl) / (collapseDim(weeklyHours) * 52.1429), "hourlyLaborCosts")
      hourlyCosts[!is.finite(hourlyCosts)] <- 0
    }

  } else {
    hourlyCosts <- calcOutput("HourlyLaborCosts", datasource = datasource, sector = sector,
                              fillWithRegression = FALSE, aggregate = FALSE)

    # calculate GDPpc [USD05MER] for regression
    gdpMERpc <- calcOutput("GDPpc",
                           scenario = "SSPs",
                           unit = "constant 2005 US$MER",
                           average2020 = FALSE,
                           naming = "scenario",
                           aggregate = FALSE)

    if (!isFALSE(projection)) {
      years <- setdiff(getItems(gdpMERpc, dim = 2), paste0("y", seq(2105, 2150, 5)))
      gdpMERpc <- gdpMERpc[, years, projection]
    } else {
      gdpMERpc <- gdpMERpc[, getItems(hourlyCosts, dim = 2), "SSP2"]
    }

    # add years with GDP data to hourlyCosts object
    hourlyCosts <- magpiesort(add_columns(hourlyCosts, dim = 2, fill = 0,
                              addnm = setdiff(getItems(gdpMERpc, dim = 2), getItems(hourlyCosts, dim = 2))))
    hourlyCosts <- hourlyCosts[, getItems(gdpMERpc, dim = 2), ]

    # set years after calibYear to 0 (as in MAgPIE we calibrate to last year of the set t_past, we need to remove data
    # for later years here as well if results should be the same)
    if (datasource == "USDA_FAO") {
      hourlyCosts[, setdiff(getItems(hourlyCosts, dim = 2), paste0("y", 1900:calibYear)), ] <- 0
    }

    # fill gaps with estimates using regression of HourlyLaborCost from ILO (US$MER05) ~ GDPpcMER
    # common slope, but calibrated to countries by shifting intercept depending on first and last hourly
    # labor cost value. Gaps within a timeseries are filled by interpolation
    regCoeff <- readSource("RegressionsILO", subtype = "HourlyLaborCosts")

    years <- getYears(hourlyCosts, as.integer = TRUE)
    for (ctry in getItems(hourlyCosts, dim = 1)) {
      ctryEst <- regCoeff[, , "intercept", drop = TRUE] + regCoeff[, , "slope", drop = TRUE] * gdpMERpc[ctry, , ]
      y <- where(hourlyCosts[ctry, , ] != 0)$true$years
      if (length(y) == 0) {
        hourlyCosts[ctry, , ] <- ctryEst
      } else {
        y <- as.integer(str_split(y, "y", simplify = TRUE)[, 2])
        yPast <- years[years < min(y)]
        yFuture <- years[years > max(y)]
        yGaps <- setdiff(years[(years >= min(y)) & (years <= max(y))], y)

        if (length(yGaps) > 0) {
          hourlyCosts[ctry, sort(c(y, yGaps)), ] <- time_interpolate(dataset = hourlyCosts[ctry, y, ],
                                                                 interpolated_year = yGaps,
                                                                 integrate_interpolated_years = TRUE)
        }

        if (length(yPast) > 0) {
          calibPast <- hourlyCosts[ctry, min(y), ] - ctryEst[, min(y), ]
          hourlyCosts[ctry, yPast, ] <- ctryEst[, yPast, ] + calibPast
        }

        if (length(yFuture) > 0) {
          calibFuture <- hourlyCosts[ctry, max(y), ] - ctryEst[, max(y), ]
          hourlyCosts[ctry, yFuture, ] <- ctryEst[, yFuture, ] + calibFuture
        }
      }
    }

    hourlyCosts[hourlyCosts < regCoeff[, , "threshold"]] <- regCoeff[, , "threshold"]
  }

  hourlyCosts <- setNames(hourlyCosts, NULL)

  # total hours worked (in calibration year for consistency with MAgPIE) as weight for aggregation to world regions
  agEmpl <- calcOutput("AgEmplILO", subsectors = TRUE, aggregate = FALSE)[, , c("Livestock", "Crops")]
  agEmpl <- if (sector != "agriculture") agEmpl[, , str_to_title(sector)] else agEmpl <- dimSums(agEmpl, dim = 3)
  weeklyHours <- calcOutput("WeeklyHoursILO", aggregate = FALSE)
  weight <- hourlyCosts
  weight[, , ] <- agEmpl[, calibYear, ] * weeklyHours[, calibYear, ]

  weight[hourlyCosts == 0] <- 0

  return(list(x = hourlyCosts,
              weight = weight,
              unit = "US$05MER",
              description = "Mean nominal hourly labour cost per employee in agriculture"))
}
