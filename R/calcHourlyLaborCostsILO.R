#' @title calcHourlyLaborCostsILO
#' @description calculates complete dataset of hourly labor costs per employee in agriculture, forestry and
#' fishery based on ILO dataset and GDPpcMER05 for regression
#' @param projections SSP on which projections should be based. If FALSE only historic values will be reported
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("HourlyLaborCostsILO")
#' }
#' @importFrom magclass getYears getSets<- getNames<- where

calcHourlyLaborCostsILO <- function(projections = FALSE) {

  ilo <- readSource("ILOSTAT", "HourlyLaborCostsByActivity")[, , c("ISIC_Rev31: A Agriculture, hunting and forestry",
                                                                   "ISIC_Rev31: B Fishing",
                                                                   "ISIC_Rev4: A Agriculture; forestry and fishing")]
  ilo <- ilo[, , "US$MER2005", drop = TRUE]

  # aggregate within rev 3.1
  mapping <- data.frame(from = c("ISIC_Rev31: A Agriculture, hunting and forestry",
                                 "ISIC_Rev31: B Fishing",
                                 "ISIC_Rev4: A Agriculture; forestry and fishing"),
                        to = c("Agriculture, forestry and fishing",
                               "Agriculture, forestry and fishing",
                               "ISIC_Rev4: A Agriculture; forestry and fishing"))
  weight <- ilo
  weight[weight != 0] <- 1

  ilo <- toolAggregate(ilo, rel = mapping, weight = weight, from = "from", to = "to", dim = 3.1)

  # aggregate the two revisions
  mapping <- data.frame(from = c("Agriculture, forestry and fishing",
                                 "ISIC_Rev4: A Agriculture; forestry and fishing"),
                        to = c("Agriculture, forestry and fishing",
                               "Agriculture, forestry and fishing"))
  weight <- ilo
  weight[weight != 0] <- 1
  ilo <- toolAggregate(ilo, rel = mapping, weight = weight, from = "from", to = "to", dim = 3.1)

  # add data for India for 2000-2017 (except 2006), based on data from the Commission of Agricultural Costs and Prices
  # (https://cacp.dacnet.nic.in/)
  hourlyCostsIndia <- c(0.2063, 0.2080, 0.2015, 0.1970, 0.1936, 0.1964, 0.2014, 0.2350, 0.2520,
                        0.2672, 0.3097, 0.3336, 0.3568, 0.3795, 0.3903, 0.3956, 0.4008)
  ilo["IND", setdiff(2000:2017, 2006), ] <- hourlyCostsIndia

  # set missing values to NA
  ilo[ilo == 0] <- NA

  # remove outliers
  ilo[ilo > 100] <- NA
  ilo[c("HRV", "MUS", "ARG"), , ] <- NA  # unreasonable low values

  # fill gaps with estimates using regression HourlyLaborCost (US$MER05) ~ GDPpcMER
  # common slope, different intercepts to calibrate to countries
  regCoeff <- readSource("RegressionsILO", "HourlyLaborCosts")

  GDPmer <- calcOutput("GDPPast", GDPPast = "IHME_USD05_MER_pc-MI", aggregate = FALSE) # mio. USD$MER05
  pop <- calcOutput("PopulationPast", aggregate = FALSE) # mio. people
  years <- intersect(intersect(getYears(GDPmer), getYears(pop)), getYears(ilo))
  GDPpcMER <- GDPmer[, years, , drop = TRUE] / pop[, years, , drop = TRUE] # GDPpcMER05

  # add future GDP for given SSP in case projections of hourly labor costs should be returned
  if (!isFALSE(projections)) {
    GDPmerFuture <- calcOutput("GDPFuture", GDPFuture = "SSPs-MI", unit = "constant 2005 US$MER", aggregate = FALSE)
    popFuture <- calcOutput("PopulationFuture", aggregate = FALSE) # mio. people
    yearsFuture <- setdiff(intersect(getItems(GDPmerFuture, dim = 2), getItems(popFuture, dim = 2)),
                     getItems(GDPpcMER, dim = 2))
    GDPpcMERfuture <- GDPmerFuture[, yearsFuture, paste0("gdp_", projections), drop = TRUE] /
                                popFuture[, yearsFuture, paste0("pop_", projections), drop = TRUE] # GDPpcMER05

    GDPpcMER <- mbind(GDPpcMER, GDPpcMERfuture)
    ilo <- add_columns(ilo, addnm = setdiff(yearsFuture, getItems(ilo, dim = 2)), dim = 2, fill = NA)
    ilo <- ilo[, getItems(GDPpcMER, dim = 2)]
  } else {
    ilo <- ilo[, years, ]
  }

  # calculate regression results
  iloEst <- regCoeff[, , "intercept", drop = TRUE] + regCoeff[, , "slope", drop = TRUE] * GDPpcMER
  getSets(iloEst) <- c("region", "year", "economic_activity")
  getNames(iloEst) <- "Agriculture, forestry and fishing"

  ilo[is.na(ilo)] <- iloEst[is.na(ilo)]

  # avoid negative and unreasonably low estimates by setting anything lower than the threshold to that value
  for (y in getItems(ilo, dim = 2)) {
    replace <- (ilo[, y, ] < regCoeff[, , "threshold"])
    ilo[, y, ][replace] <- regCoeff[, , "threshold"][replace]
  }

  # total hours worked as weight for aggregation to world regions
  agEmpl <- calcOutput("AgEmplILO", aggregate = FALSE, subsectors = FALSE)
  weeklyHours <- calcOutput("WeeklyHoursILO", aggregate = FALSE)
  weight <- agEmpl * weeklyHours

  # keeping weights constant for future years (for projections)
  years <- setdiff(getYears(ilo), getYears(weight))
  if (length(years) > 0) {
    weight <- time_interpolate(weight, interpolated_year = years,
                               integrate_interpolated_years = TRUE, extrapolation_type = "constant")
  }
  weight <- weight[, getItems(ilo, dim = 2), ]

  return(list(x = ilo,
              weight = weight,
              unit = "US$05MER",
              description = "Mean nominal hourly labour cost per employee by economic activity"))
}
