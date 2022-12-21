#' @title calcNuePasture
#' @description calculates the soil nitrogen uptake efficiency of pastures. #
#' This is the nitrogen taken up from the soil (N in crop biomass minus biological
#' fixation minus seed N) divided by the soil N inputs (fertilizer, manure etc).
#' For the future, NUE scenarios are added.
#' @param cellular cellular disaggreagation or national values
#' @param maccbase whether future scenarios should be expressed as base efficiency,
#' excluding additional macc improvements (new default)
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcSNUpE()]
#' [calcNitrogenBudgetPasture()]
#' @examples
#' \dontrun{
#' calcOutput("NuePasture")
#' }
#'
calcNuePasture <- function(cellular = FALSE, maccbase = TRUE) {
  a <- calcOutput("NitrogenBudgetPasture", aggregate = FALSE, cellular = cellular, deposition = "Nsurplus2")
  outputs <- c(
    "harvest"
  )
  inputs <- c(
    "fixation_freeliving",
    "grazing", "fertilizer", "deposition",
    "balanceflow"
  )
  outputs <- dimSums(a[, , outputs], dim = 3.1)
  inputs <- dimSums(a[, , inputs], dim = 3.1)
  NUE <- outputs / inputs #nolint

  # future

  if (maccbase == FALSE) {
    zhang <- readSource("Zhang2015")
    data <- toolNUEscenarios(x = NUE, weight = inputs, zhang = zhang)
    weight <- data$weight
    out <- data$x
  } else {
    x <- setNames(toolHoldConstantBeyondEnd(NUE), "constant")
    weight <- setNames(toolHoldConstantBeyondEnd(inputs), NULL)

    # Pastures
    #- increase minimum NUE to 65% to avoid fertilizer application where no fertilizer is needed
    scenarioname <- "constant_min55_min60_min65"
    x <- add_columns(x, addnm = scenarioname, dim = 3.1)
    y2020 <- setYears(x[, "y2010", "constant"], NULL)
    y2020[y2020 < 55 / 100] <- 55 / 100
    y2050 <- setYears(x[, "y2010", "constant"], NULL)
    y2050[y2050 < 60 / 100] <- 60 / 100
    y2100 <- setYears(x[, "y2010", "constant"], NULL)
    y2100[y2100 < 65 / 100] <- 65 / 100
    x[, , scenarioname] <- convergence(
      origin = x[, , "constant"],
      aim = y2020, start_year = "y2010",
      end_year = "y2020", type = "linear"
    )
    x[, , scenarioname] <- convergence(
      origin = x[, , scenarioname], aim = y2050, start_year = "y2020", end_year = "y2050", type = "linear"
    )
    x[, , scenarioname] <- convergence(
      origin = x[, , scenarioname], aim = y2100, start_year = "y2050", end_year = "y2100", type = "linear"
    )

    out <- x
    out[is.na(out)] <- 0.5
    out[is.infinite(out)] <- 0.5
  }

  return(list(
    x = out,
    weight = weight,
    unit = "Share",
    description = "Soil nitrogen uptake efficiency",
    isocountries = !cellular
  ))
}
