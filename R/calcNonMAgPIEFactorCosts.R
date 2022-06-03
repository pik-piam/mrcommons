#' @title calcNonMAgPIEFactorCosts
#' @description Calculates factor costs that should affect agricultural employment but are not included in MAgPIE
#' factor costs
#' @param subtype either factor cost share of "subsidies" (which don't enter MAgPIE labor costs as they should not
#' affect prices), or of "missingVoP" (which refers to livestock VoP that can't be mapped to MAgPIE livestock
#' categories, i.e. wool, beeswax, honey, silk-worms)
#' @param aggSubsidies boolean: if subtype is "subsidies", should crop and livestock subsidies be reported separately
#' or as aggregate?
#' @param extrapolate boolean: should values be extrapolate (by keeping constant) until 2150?
#' @return magpie object. in mio. USDMER05
#' @author Debbora Leip
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("NonMAgPIEFactorCosts", subtype = "subsidies")
#' }
#'
calcNonMAgPIEFactorCosts <- function(subtype = "subsidies", aggSubsidies = FALSE, extrapolate = TRUE) {

  # factor cost share
  fractionCrops <- calcOutput("FractionInputsUSDA", products = "kcr", aggregate = FALSE)
  fractionLivst <- calcOutput("FractionInputsUSDA", products = "kli", aggregate = FALSE)
  fractionCrops <- setNames(dimSums(fractionCrops[, , c("Labor", "Capital")], dim = 3), "Crops")
  fractionLivst <- setNames(dimSums(fractionLivst[, , c("Labor", "Capital")], dim = 3), "Livestock")
  fractions <- mbind(fractionCrops, fractionLivst)

  if (subtype == "subsidies") { # ag. subsidies in mio.USD05MER
    out <- calcOutput("IFPRIsubsidy", aggregate = FALSE, fillGaps = TRUE)

    # get factor cost share of subsidies
    addYearsFraction <- setdiff(getItems(out, dim = 2), getItems(fractions, dim = 2))
    fractions <- time_interpolate(dataset = fractions,
                                  interpolated_year = addYearsFraction,
                                  integrate_interpolated_years = TRUE,
                                  extrapolation_type = "constant")[, getItems(out, dim = 2), ]
    out <- out * fractions

    # extrapolate
    if (isTRUE(extrapolate)) {
      addYears <- setdiff(paste0("y", seq(1965, 2150, 5)), getItems(out, dim = 2))
      out <- time_interpolate(dataset = out,
                              interpolated_year = addYears,
                              integrate_interpolated_years = TRUE,
                              extrapolation_type = "constant")
    }

    if (aggSubsidies) out <- dimSums(out, dim = 3)

  } else if (subtype == "missingVoP") {
    out <- calcOutput("VoPlivst", other = TRUE, aggregate = FALSE)[, , "livst_other", drop = TRUE]
    out <- out[, 1991:2017, ] # no data before 1991, last two years incomplete

    # get factor cost share of VoP
    addYearsFraction <- setdiff(getItems(out, dim = 2), getItems(fractions, dim = 2))
    fractions <- time_interpolate(dataset = fractions,
                                  interpolated_year = addYearsFraction,
                                  integrate_interpolated_years = TRUE,
                                  extrapolation_type = "constant")[, getItems(out, dim = 2), "Livestock"]
    out <- out * collapseDim(fractions, dim = 3)

    # extrapolate
    if (isTRUE(extrapolate)) {
      addYears <- setdiff(paste0("y", seq(1965, 2150, 5)), getItems(out, dim = 2))
      out <- time_interpolate(dataset = out,
                            interpolated_year = addYears,
                            integrate_interpolated_years = TRUE,
                            extrapolation_type = "constant")
    }

  }

  return(list(x = out,
              weight = NULL,
              unit = "mio. USDMER05",
              description = "Factor costs affecting employment but not included in MAgPIE factor costs"))
}
