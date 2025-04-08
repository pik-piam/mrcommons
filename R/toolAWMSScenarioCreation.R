#' @title toolAWMSScenarioCreation
#' @description Tool function to calculate the share of manure managed in different animal waste
#'              management systems in confinements. This function creates AWMS scenarios by applying
#'              absolute target shares and relative changes to baseline shares, then distributing any
#'              remaining manure proportions among non-target AWMS items.
#'
#' @param name Character. Name of the scenario.
#' @param startYear The year when prediction starts.
#' @param targetYears Target years for applying AWMS scenario adjustments.
#' @param targetAWMS Identifiers for the AWMS items to be targeted with scenario adjustments.
#' @param typeAWMS Specifies the type of target adjustment for each AWMS in targetAWMS.
#'        Acceptable values are "target" for absolute target values and "relative_change" for adjustments
#'        relative to baseline.
#' @param valuesAWMS List. Each element corresponds to a target year and contains a numeric vector of
#'        target values for each AWMS in targetAWMS. For "target" types, the value represents the absolute
#'        target share; for "relative_change" types, the value is a multiplicative factor applied to baseline shares.
#' @param out a magclass object containing historical data, which is used to extract baseline values and to
#'            propagate the scenario convergence.
#'
#' @return a magclass object
#'
#' @author Michael Crawford, Edna J. Molina Bacca
#' @seealso [calcAWMSconfShr()]

toolAWMSScenarioCreation <- function(name, startYear, targetYears, targetAWMS, typeAWMS, valuesAWMS, out) {

  # Get all AWMS items and define non-target items
  allAWMS <- getItems(out, dim = 3.3)
  restAWMS <- setdiff(allAWMS, targetAWMS)

  # Baseline state from the starting year and initialize convergedState
  baselineState <- out[, startYear, "constant"]
  convergedState <- out[, , "constant"]
  currentStartYear <- startYear

  for (yearIndex in seq_along(targetYears)) {
    # Begin each target year with the baseline state
    updatedState <- baselineState

    absoluteSum <- dimSums(baselineState, dim = 3.3)
    absoluteSum[] <- 0

    totalPotentialRelative <- dimSums(baselineState, dim = 3.3)
    totalPotentialRelative[] <- 0

    # Pre-allocate vector for potential relative values (one for each target AWMS)
    potentialRelativeShares <- baselineState
    potentialRelativeShares[] <- 0

    # Calculate target shares
    for (i in seq_along(targetAWMS)) {
      target <- targetAWMS[i]

      # Get the current value (absolute target or relative factor) for this target AWMS
      currentValue <- valuesAWMS[[yearIndex]][i]

      if (typeAWMS[i] == "target") {
        # For absolute targets, set directly
        updatedState[, , target] <- currentValue
        absoluteSum <- absoluteSum + currentValue
      } else if (typeAWMS[i] == "relative_change") {
        # For relative changes, compute the potential new share from baseline
        potentialValue <- baselineState[, , target] * currentValue
        potentialRelativeShares[, , target] <- potentialValue
        totalPotentialRelative <- totalPotentialRelative + dimSums(potentialValue, dim = 3.3)
      } else {
        stop("Invalid type provided for AWMS scenario creation")
      }
    }

    # Calculate available share for relative changes after absolute targets are allocated
    availableSpace <- 1 - absoluteSum
    mstools::toolExpectTrue(all(availableSpace >= 0), "Summed AWMS targets do not exceed 1")

    # Determine if relative changes need to be scaled
    scaleFactor <- totalPotentialRelative
    scaleFactor[] <- 1
    toScale <- totalPotentialRelative > availableSpace
    scaleFactor[toScale] <- (availableSpace / totalPotentialRelative)[toScale]

    # Apply scaling to relative targets
    for (i in seq_along(targetAWMS)) {
      target <- targetAWMS[i]
      if (typeAWMS[i] == "relative_change") {
        updatedState[, , target] <- potentialRelativeShares[, , target] * scaleFactor
      }
    }

    # Total share allocated to target AWMS
    totalTargetShare <- absoluteSum + (totalPotentialRelative * scaleFactor)

    # Calculate remaining share and distribute it among non-target AWMS proportionally to baseline shares
    residualShare <- 1 - totalTargetShare
    normalizedRestProportions <- baselineState[, , restAWMS] / dimSums(baselineState[, , restAWMS], dim = 3.3)
    updatedState[, , restAWMS] <- normalizedRestProportions * residualShare

    # Validation
    mstools::toolExpectTrue(
      all(abs(dimSums(updatedState, dim = 3.3) - 1) < .Machine$double.eps^0.5),
      "AWMS shares must sum to 1"
    )

    mstools::toolExpectTrue(
      all(updatedState >= 0 & updatedState <= 1),
      "AWMS shares must be in the range [0, 1]"
    )

    # Propagate the updated state to the target year via convergence
    convergedState <- convergence(
      origin     = convergedState,
      aim        = updatedState,
      start_year = currentStartYear,
      end_year   = targetYears[[yearIndex]],
      direction  = NULL,
      type       = "linear"
    )
    currentStartYear <- targetYears[[yearIndex]]
  }

  # Set scenario name and bind the new converged state with the original output
  getNames(convergedState, dim = 1) <- name
  return(mbind(out, convergedState))
}
