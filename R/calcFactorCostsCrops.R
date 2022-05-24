#' @title calcFactorCostsCrops
#' @description calculates factor costs for crop production in mio. US$MER05
#' @param datasource only source available is "USDA" (calculates factor costs by applying factor cost share from USDA
#' to VoP from FAO)
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("FactorCostsCrops")
#' }
#' @importFrom magclass setNames dimSums time_interpolate

calcFactorCostsCrops <- function(datasource = "USDA") {

  if (datasource == "USDA") {
    # Value of Production for livestock in US$MER2005 (including FAO livst categories not mapped to MAgPIE categories)
    vopCrops <- calcOutput("VoPcrops", fillGaps = TRUE, aggregate = FALSE) # mio. US$MER05

    # no VoP data before 1991, data for 2019 incomplete
    years <- setdiff(getYears(vopCrops, as.integer = TRUE), c(1960:1990, 2019))

    # USDA labor cost shares
    shares <- calcOutput("FractionInputsUSDA", products = "kcr", aggregate = FALSE)
    shares <- dimSums(shares[, , c("Labor", "Capital")], dim = 3)

    # closest 5-year step before and after start of VoP data needed for interpolation of shares
    y <- intersect(paste0("y", seq(min(years) - min(years) %% 5, max(years) - max(years) %% 5 + 5, 5)),
                   getItems(shares, dim = 2))

    # filling missing values with region average, using production as weight
    h12 <- toolGetMapping("regionmappingH12.csv", type = "regional")
    weight <- dimSums(collapseDim(calcOutput("Production", products = "kcr", aggregate = FALSE)[, , "dm"]), dim = 3.1)
    weight <- time_interpolate(weight, interpolated_year = setdiff(y, getItems(weight, dim = 2)),
                               extrapolation_type = "constant", integrate_interpolated_years = TRUE)[, y, ]
    shares <- toolFillWithRegionAvg(shares[, y, ], valueToReplace = 0, weight = weight,
                                    regionmapping = h12, verbose = FALSE, warningThreshold = 1)

    # for REF in 1990 no country has a value, so toolFillWithRegionAvg assigns NA. Use values from 1995 instead:
    if ("y1990" %in% y) { # subsidy data starts only in 2005
      ref <- h12$CountryCode[h12$RegionCode == "REF"]
      shares[ref, 1990, ]  <- shares[ref, 1995, ]
    }

    # interpolate between the five-year-steps
    shares <- time_interpolate(shares,
                               interpolated_year = setdiff(years, getYears(shares, as.integer = TRUE)),
                               extrapolation_type = "constant",
                               integrate_interpolated_years = TRUE)[, years, ]

    # estimate total labor costs as share of VoP
    out <- vopCrops[, years, ] * shares
  } else {
    stop("Datasource not available")
  }

  return(list(x = out,
              weight = NULL,
              unit = "mio. USD05MER",
              description = "Factor costs for crop production"))
}
