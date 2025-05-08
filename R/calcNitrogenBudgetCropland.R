#' @title calcNitrogenBudgetCropland
#' @description Calculates Nitrogen Budgets for Cropland soils on country levels.
#'
#' @param deposition if FALSE, deposition is not accounted for in the distribution.
#' Use FALSE to avoid circularities in calcNitrogenBudget
#' @param include_fertilizer including fertilizer in budget. Use FALSE to avoid circularities in specific cases
#' @param max_snupe NULL or a numeric value. if numeric, an additional N balanceflow is included that takes care
#' that the soil nitrogen uptake efficiency does not exceed the numeric value in balanceflow.
#' @param cellular disaggregated to 0.5 degree grid
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("NitrogenBudgetCropland")
#' }
#' @importFrom magclass setNames



calcNitrogenBudgetCropland <- function(cellular = FALSE,
                                       deposition = "CEDS",
                                       include_fertilizer = TRUE, # nolint: object_name_linter.
                                       max_snupe = 0.85) { # nolint: object_name_linter.
  past <- findset("past")

  harvest <- dimSums(calcOutput("Production", products = "kcr", cellular = cellular,
                                calibrated = TRUE, aggregate = FALSE)[, past, "nr"], dim = 3)
  ag <- collapseNames(calcOutput("ResFieldBalancePast", aggregate = FALSE, cellular = cellular)[, past, "nr"])
  bg <- dimSums(collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                         plantparts = "bg", aggregate = FALSE)[, past, "nr"]), dim = 3.1)

  seed <- dimSums(calcOutput("Seed", cellular = cellular, products = "kcr", aggregate = FALSE)[, past, "nr"], dim = 3)

  fixation <- dimSums(calcOutput("NitrogenFixationPast", fixation_types = "both", sum_plantparts = TRUE,
                                 aggregate = FALSE, cellular = cellular), dim = 3.2)
  som <- calcOutput("SOMlossN", cellular = cellular, aggregate = FALSE)[, past, ]

  if (include_fertilizer == TRUE) {
    fertilizer <- calcOutput("FertN", aggregate = FALSE, appliedto = "crop", cellular = cellular,
                             deposition = deposition, max_snupe = max_snupe)[, past, ]
    fertilizer <- setNames(fertilizer, "fertilizer")
  } else {
    fertilizer <- NULL
  }

  manure <- collapseNames(calcOutput("ManureRecyclingCroplandPast", aggregate = FALSE, cellular = cellular)[, , "nr"])
  manureCroplandGrazing <- collapseNames(dimSums(calcOutput("Excretion",
                                                            cellular = cellular,
                                                            aggregate = FALSE)[, , "stubble_grazing"][, , "nr"],
                                                 dim = 3.2))
  adeposition <- setNames(collapseNames(dimSums(calcOutput("AtmosphericDeposition",
                                                           datasource = deposition,
                                                           cellular = cellular,
                                                           aggregate = FALSE)[, past, "crop"],
                                                dim = c(3.4))), "deposition")
  if (!cellular) adeposition["ATA", , ] <- 0

  outputs <- mbind(setNames(harvest, "harvest"),
                   setNames(collapseNames(ag[, , "biomass"]), "ag"),
                   setNames(bg, "bg"))

  inputsDirect <- mbind(setNames(seed, "seed"),
                        setNames(fixation[, , "fixation_crops"], "fixation_crops"))

  inputs <- mbind(setNames(fixation[, , "fixation_freeliving"], "fixation_freeliving"),
                  setNames(manure, "manure_conf"),
                  setNames(manureCroplandGrazing, "manure_stubble_grazing"),
                  setNames(collapseNames(ag[, , "recycle"]), "ag_recycling"),
                  setNames(bg, "bg_recycling"),
                  setNames(collapseNames(ag[, , "ash"]), "ag_ash"),
                  setNames(adeposition, "deposition"),
                  setNames(som, "som"),
                  fertilizer)

  # Balanceflow based on assumption that everything above max_snupe on country level is definetly a bug
  # For cellular calculation same trashhold will be used
  if (!is.null(max_snupe)) {
    balanceflow <- (dimSums(outputs, dim = 3.1) - dimSums(inputsDirect, dim = 3.1)) /
      max_snupe - dimSums(inputs, dim = 3.1)
    balanceflow[balanceflow < 0] <- 0

  } else {
    balanceflow <- dimSums(outputs, dim = 3.1) * 0
  }

  balanceflow <- setNames(balanceflow, "balanceflow")
  surplus <- setNames(dimSums(inputs, dim = 3) +
                        dimSums(inputsDirect, dim = 3) +
                        dimSums(balanceflow, dim = 3) -
                        dimSums(outputs, dim = 3), "surplus")
  out <- mbind(outputs, inputsDirect, inputs, balanceflow, surplus)

  return(list(x = out,
              weight = NULL,
              unit = "Mt Nr",
              description = "Nitrogen budget on croplands for historical period",
              isocountries = !cellular))
}
