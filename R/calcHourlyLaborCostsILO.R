#' @title calcHourlyLaborCostsILO
#' @description calculates complete dataset of hourly labor costs per employee in agriculture, forestry and
#' fishery based on ILO dataset and GDPpcMER05 for regression
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("HourlyLaborCostsILO")
#' }
#' @importFrom magclass getYears getSets<- getNames<- where

calcHourlyLaborCostsILO <- function() {
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

  # set missing values to NA
  ilo[ilo == 0] <- NA

  # remove outliers
  ilo[ilo > 100] <- NA
  ilo[c("HRV", "MUS", "ARG"), , ] <- NA  # unreasonable low values
  minObs <- min(ilo, na.rm = TRUE)

  # fill gaps with estimates using regression HourlyLaborCost (US$MER05) ~ GDPpcMER
  # fixed effect regression to calibrate to different countries -> common slope, different intercepts
  regCoeff <- readSource("RegressionsILO", "HourlyLaborCosts")

  GDPmer <- calcOutput("GDPPast", subtype = "IHME_USD05_MER_pc-MI", aggregate = FALSE) # mio. USD$MER05
  pop <- calcOutput("PopulationPast", aggregate = FALSE) # mio. people
  years <- intersect(intersect(getYears(GDPmer), getYears(pop)), getYears(ilo))
  GDPpcMER <- GDPmer[, years, , drop = TRUE] / pop[, years, , drop = TRUE] # GDPpcMER05

  iloEst <- regCoeff[, , "intercept", drop = TRUE] + regCoeff[, , "slope", drop = TRUE] * GDPpcMER
  getSets(iloEst) <- c("region", "year", "economic_activity")
  getNames(iloEst) <- "Agriculture, forestry and fishing"

  ilo <- ilo[, years, ]
  ilo[is.na(ilo)] <- iloEst[is.na(ilo)]

  # avoid negative and unreasonably low estimates by setting anything lower than the lowest observed value to minObs
  ilo[ilo < minObs] <- minObs

  # reduce time series to years that have data
  ilo <- ilo[, where(is.finite(ilo))$true$years, ]

  # ag. empl. as weight for aggregation to world regions
  weight <- calcOutput("AgEmplILO", aggregate = FALSE)
  years <- intersect(getYears(ilo), getYears(weight))
  weight <- weight[, years, ]
  ilo <- ilo[, years, ]

  return(list(x = ilo,
              weight = weight,
              unit = "US$05MER",
              description = "Mean nominal hourly labour cost per employee by economic activity"))
}
