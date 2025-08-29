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
calcNitrogenBudgetCropland <- function(cellular = FALSE,
                                       deposition = "CEDS",
                                       include_fertilizer = TRUE, # nolint: object_name_linter.
                                       max_snupe = 0.85) { # nolint: object_name_linter.
  harvest <- dimSums(calcOutput("Production", products = "kcr", cellular = cellular,
                                calibrated = TRUE, aggregate = FALSE)[, , "nr"], dim = 3)
  ag <- collapseNames(calcOutput("ResFieldBalancePast", aggregate = FALSE, cellular = cellular)[, , "nr"])
  bg <- dimSums(collapseNames(calcOutput("ResBiomass", cellular = cellular,
                                         plantparts = "bg", aggregate = FALSE)[, , "nr"]), dim = 3.1)
  seed <- dimSums(calcOutput("Seed", cellular = cellular, products = "kcr", aggregate = FALSE)[, , "nr"], dim = 3)
  fixation <- dimSums(calcOutput("NitrogenFixationPast", fixation_types = "both", sum_plantparts = TRUE,
                                 aggregate = FALSE, cellular = cellular), dim = 3.2)
  som <- calcOutput("SOMlossN", cellular = cellular, aggregate = FALSE)

  if (cellular) {
    som <- toolCell2isoCell(som)
  }

  if (include_fertilizer) {
    fertilizer <- calcOutput("FertN", aggregate = FALSE, appliedto = "crop", cellular = cellular,
                             deposition = deposition, max_snupe = max_snupe)
    fertilizer <- setNames(fertilizer, "fertilizer")
    cyears <- intersect(getYears(fertilizer), findset("past_til2020"))
    fertilizer <- fertilizer[, cyears, ]
  } else {
    fertilizer <- NULL
    cyears <- intersect(getYears(som), findset("past_til2020"))
  }

  harvest <- harvest[, cyears, ]
  ag <- ag[, cyears, ]
  bg <- bg[, cyears, ]
  seed <- seed[, cyears, ]
  fixation <- fixation[, cyears, ]
  som <- som[, cyears, ]

  manure <- collapseNames(calcOutput("ManureRecyclingCroplandPast", aggregate = FALSE,
                                     cellular = cellular)[, cyears, "nr"])
  manureCroplandGrazing <- collapseNames(dimSums(calcOutput("Excretion", cellular = cellular,
                                                            aggregate = FALSE)[, , "stubble_grazing"][, cyears, "nr"],
                                                 dim = 3.2))
  adeposition <- setNames(collapseNames(dimSums(calcOutput("AtmosphericDeposition", datasource = deposition,
                                                           cellular = cellular,
                                                           aggregate = FALSE)[, cyears, "crop"], dim = 3.4)),
                          "deposition")
  if (!cellular) {
    adeposition["ATA", , ] <- 0
  }

  outputs <- mbind(setNames(harvest, "harvest"),
                   setNames(collapseNames(ag[, , "biomass"]), "ag"),
                   setNames(bg, "bg"))

  inputsDirect <- mbind(setNames(seed, "seed"),
                        setNames(fixation[, , "fixation_crops"], "fixation_crops"))

  inputs <- mbind(
    setNames(fixation[, , "fixation_freeliving"], "fixation_freeliving"),
    setNames(manure, "manure_conf"),
    setNames(manureCroplandGrazing, "manure_stubble_grazing"),
    setNames(collapseNames(ag[, , "recycle"]), "ag_recycling"),
    setNames(bg, "bg_recycling"),
    setNames(collapseNames(ag[, , "ash"]), "ag_ash"),
    setNames(adeposition, "deposition"),
    setNames(som, "som"),
    fertilizer
  )

  # Balanceflow based on assumption that everything above max_snupe on country level is definitely a bug
  # For cellular calculation same threshold will be used
  if (!is.null(max_snupe)) {
    outMinusIn <- dimSums(outputs, dim = 3.1) - dimSums(inputsDirect, dim = 3.1)
    balanceflow <- (outMinusIn / max_snupe) - dimSums(inputs, dim = 3.1)
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

  return(list(
              x = out,
              weight = NULL,
              unit = "Mt Nr",
              description = "Nitrogen budget on croplands for historical period",
              isocountries = !cellular))
}
