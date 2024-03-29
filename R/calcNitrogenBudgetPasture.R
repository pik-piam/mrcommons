#' @title calcNitrogenBudgetPasture
#' @description Calculates Nitrogen Budgets for Pasture soils on country levels.
#' @param include_fertilizer including fertilizer in budget. Use FALSE to avoid circularities in specific cases
#' @param deposition if FALSE, deposition is not accounted for in the distribution.
#' Use FALSE to avoid circularities in calcNitrogenBudget
#' @param max_nue NULL or a numeric value. if numeric, an additional N balanceflow is included that
#' takes care that the nitrogen use efficiency does not exceed the numeric value in balanceflow.
#' @param cellular cellular disaggreagation or national values
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("NitrogenBudgetPasture")
#' }
#' @importFrom magclass setNames



calcNitrogenBudgetPasture <- function(cellular = FALSE,
                                      include_fertilizer = TRUE, # nolint: object_name_linter.
                                      deposition = "CEDS",
                                      max_nue = 0.9) { # nolint: object_name_linter.
  past <- findset("past")

  harvest <- collapseNames(calcOutput("Production", products = "pasture", cellular = cellular,
                                      aggregate = FALSE)[, past, "nr"])
  excretion <- collapseNames(dimSums(calcOutput("Excretion", cellular = cellular,
                                                aggregate = FALSE)[, , "grazing"][, , "nr"], dim = 3.2))
  fixation <- collapseNames(calcOutput("NitrogenBNF", cellular = cellular, aggregate = FALSE)[, , "past"])
  if (include_fertilizer == TRUE) {
    fertilizer <- calcOutput("FertN", aggregate = FALSE, appliedto = "past", cellular = cellular,
                             deposition = deposition, max_snupe = max_nue)[, past, ]
    fertilizer <- setNames(fertilizer, "fertilizer")
  } else {
    fertilizer <- NULL
  }
  # som missing

  adeposition <- setNames(
    collapseNames(dimSums(calcOutput("AtmosphericDeposition", datasource = deposition, cellular = cellular,
                                     aggregate = FALSE)[, past, "past"], dim = c(3.4))),
    "deposition")
  if (!cellular) adeposition["ATA", , ] <- 0  ### Antarctica has large deposition but no icefree land

  outputs <- mbind(
    setNames(harvest, "harvest")
  )

  inputs <- mbind(
    setNames(fixation, "fixation_freeliving"),
    setNames(excretion, "grazing"),
    setNames(adeposition, "deposition"),
    fertilizer
  )

  # Balanceflow based on assumption that everything above max_nue on country level is definitely a bug
  # For cellular calculation same threshold will be used
  if (!is.null(max_nue)) {
    balanceflow <- (dimSums(outputs, dim = 3.1)) / max_nue - dimSums(inputs, dim = 3.1)
    balanceflow[balanceflow < 0] <- 0

  } else {
    balanceflow <- dimSums(outputs, dim = 3.1) * 0
  }

  balanceflow <- setNames(balanceflow, "balanceflow")
  surplus <- setNames(dimSums(inputs, dim = 3) + dimSums(balanceflow, dim = 3) - dimSums(outputs, dim = 3), "surplus")
  out <- mbind(outputs, inputs, balanceflow, surplus)

  return(list(
    x = out,
    weight = NULL,
    unit = "Mt Nr",
    description = "Nitrogen budget on pastures for historical period",
    isocountries = !cellular))
}
