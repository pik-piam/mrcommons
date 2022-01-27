#' @title calcLaborCosts
#' @description calculates total labor costs in mio. US$MER05 (coverage depends on source: crop and livestock
#' production for USDA, and additionally forestry and fishery for GTAP and ILO)
#' @param datasource data source on which the labor costs should be based. Available are ILO, USDA (which also uses data
#' on VoP from FAO), and GTAP
#' @param subsectors boolean: should output be aggregated or split into available subsectors (crops, livst, forestry,
#' fishery)
#' @param gtapVar variable name to use from GTAP (only relevant if source is "GTAP")
#' @param addSubsidies boolean: should subsidy data (from IFPRI) should be added to VoP before applying USDA cost
#' shares (only relevant if datasource is "USDA")
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("LaborCosts", datasource = "ILO")
#' }
#' @importFrom magclass setNames dimSums time_interpolate
#' @importFrom GDPuc convertGDP

calcLaborCosts <- function(datasource = "ILO", subsectors = TRUE, gtapVar = "NVFA", addSubsidies = FALSE) {

  # get data from specified source
  if (datasource == "ILO") {

    # ILO employment in agriculture (mio. people)
    iloEmpl <- calcOutput("AgEmplILO", aggregate = FALSE, subsectors = subsectors)

    # ILO mean weekly hours actually worked per employed person in agriculture (h)
    iloWeeklyHours <- calcOutput("WeeklyHoursILO", aggregate = FALSE)

    # ILO mean nominal hourly labor cost per employee in agriculture (US$05MER/h)
    iloLaborCosts <- calcOutput("HourlyLaborCostsILO", aggregate = FALSE)

    # subset to joint years
    years <- intersect(intersect(getItems(iloEmpl, dim = 2), getItems(iloWeeklyHours, dim = 2)),
                       getItems(iloLaborCosts, dim = 2))
    iloEmpl <- iloEmpl[, years, ]
    iloWeeklyHours <- iloWeeklyHours[, years, ]
    iloLaborCosts <- iloLaborCosts[, years, ]

    # combine data sets to get total labor costs
    iloTotalHours <- 52.1429 * iloWeeklyHours[, , , drop = TRUE] * iloEmpl # mio. hours
    out <- iloTotalHours * iloLaborCosts[, , , drop = TRUE] # mio. US$MER05

  } else if (datasource == "USDA") {

    # Value of Production for livestock in US$MER2005 (including FAO livst categories not mapped to MAgPIE categories)
    VoP_livst <- calcOutput("VoP_livst", other = TRUE, aggregate = FALSE) # mio. US$MER05
    VoP_livst <- setNames(dimSums(VoP_livst, dim = 3), "Livestock")

    # Value of Production for crops in US$MER2005
    VoP_crops <- calcOutput("VoP_crops", output = "absolute", aggregate = FALSE) # mio. US$MER05
    VoP_crops <- setNames(dimSums(VoP_crops, dim = 3), "Crops")

    years <- setdiff(intersect(getItems(VoP_livst, dim = 2), getItems(VoP_crops, dim = 2)), paste0("y", 1960:1989))

    # add subsidies to VoP
    if (isTRUE(addSubsidies)) {
      subsidies <- calcOutput("IFPRIsubsidy", aggregate = FALSE)
      years <- intersect(years, getItems(subsidies, dim = 2))
      VoP_crops[, years, ] <- VoP_crops[, years, ] + subsidies[, years, "Crops"]
      VoP_livst[, years, ] <- VoP_livst[, years, ] + subsidies[, years, "Livestock"]
    }

    # combine VoP for crops and livestock
    VoP_agriculture <- mbind(VoP_livst, VoP_crops)
    VoP_agriculture[!is.finite(VoP_agriculture)] <- 0

    # USDA labor cost shares
    shares_kcr <- calcOutput("FractionInputsUSDA", products = "kcr", aggregate = FALSE)[, , "Labor"]
    shares_kli <- calcOutput("FractionInputsUSDA", products = "kli", aggregate = FALSE)[, , "Labor"]
    y <- intersect(years, getItems(shares_kcr, dim = 2))

    # filling missing values with region average, using production as weight
    h12 <- toolGetMapping("regionmappingH12.csv", type = "regional")
    weight <- dimSums(collapseDim(calcOutput("Production", aggregate = FALSE)[, , "dm"]), dim = 3.1)
    weight <- time_interpolate(weight, interpolated_year = setdiff(y, getItems(weight, dim = 2)),
                               extrapolation_type = "constant", integrate_interpolated_years = TRUE)[, y, ]
    shares_kcr <- toolFillWithRegionAvg(shares_kcr[, y, ], valueToReplace = 0, weight = weight,
                                        regionmapping = h12, verbose = FALSE)
    shares_kli <- toolFillWithRegionAvg(shares_kli[, y, ], valueToReplace = 0, weight = weight,
                                        regionmapping = h12, verbose = FALSE)
    shares <- setNames(mbind(shares_kli, shares_kcr), c("Livestock", "Crops"))

    # for REF in 1990 no country has a value, so toolFillWithRegionAvg assigns NA. Use values from 1995 instead:
    if ("y1990" %in% years) { # subsidy data starts only in 2005
      ref <- h12$CountryCode[h12$RegionCode == "REF"]
      shares[ref, 1990, ]  <- shares[ref, 1995, ]
    }

    # interpolate between the five-year-steps
    shares <- time_interpolate(shares,
                               interpolated_year = setdiff(years, getYears(shares)),
                               extrapolation_type = "constant",
                               integrate_interpolated_years = TRUE)[, years, ]

    # estimate total labor costs as share of VoP
    out <- VoP_agriculture[, years, ] * shares

    # aggregate if subsectors == FALSE
    if (isFALSE(subsectors)) out <- setNames(dimSums(out, dim = 3), "Agriculture")

  } else if (datasource == "GTAP") {

    # gtap data in mio. current US$MER
    if (gtapVar == "NVFA") {
      gtap <- dimSums(readSource("GTAP81", "SF01"), dim = c("DIR", "PURCHVALUE"))
    } else if (gtapVar == "VFM") {
      gtap <- readSource("GTAP81", "VFM")
      getSets(gtap) <- c("REG", "year", "DEMD_COMM", "PROD_COMM")
    } else if (gtapVar == "EVFA") {
      gtap <- readSource("GTAP81", "AG03")
      getSets(gtap) <- c("REG", "year", "DEMD_COMM", "PROD_COMM")
    } else {
      stop("This GTAP variable is not available for labor costs")
    }

    labor <- c("UnSkLab", "SkLab")

    # https://www.gtap.agecon.purdue.edu/databases/contribute/detailedsector.asp
    kcr <- c("pdr", "wht", "gro", "v_f", "osd", "c_b", "pfb", "ocr")
    kli <- c("ctl", "oap", "rmk")
    fish <- c("fsh")
    forest <- c("frs")

    .getSector <- function(items, name) {
      return(setNames(dimSums(gtap[, , list("DEMD_COMM" = labor, "PROD_COMM" = items)],
                              dim = c("DEMD_COMM", "PROD_COMM")), name))
    }

    laborCosts <- mbind(Map(.getSector, items = list(kcr, kli, fish, forest),
                               name = list("Crops", "Livestock", "Fisheries", "Forestry")))

    # aggregate if subsectors == FALSE
    if (isFALSE(subsectors)) laborCosts <- setNames(dimSums(laborCosts, dim = 3), "Agriculture, forestry and fishing")

    # convert to USDMER05 (for countries missing the inflation factor, we assume no inflation)
    out <- GDPuc::convertGDP(laborCosts, unit_in = "current US$MER", unit_out = "constant 2005 US$MER")
    out[is.na(out)] <- laborCosts[is.na(out)]

  } else {
    stop("Data source not available")
  }

  return(list(x = out,
              weight = NULL,
              unit = "mio. USD05MER",
              description = "labor costs in agriculture"))
}
