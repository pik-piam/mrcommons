#' @title calcEmisNitrogenPreagriculture
#' @description
#' Calculates nitrogenous emissions Nitrogen emissions from soils under 100% natural cover
#' (even for crop and urban) assuming a pre-agricultural time.
#'
#' @param cellular cellular or country outputs
#' @param deposition if TRUE, losses include atmospheric deposition inputs that are lost afterwards.
#' If false, only biological fixation is considered.
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcEmisNitrogenPast()],
#' [calcExcretion()]
#' @examples
#' \dontrun{
#' calcOutput("EmisNitrogenPreagriculture")
#' }
#'
calcEmisNitrogenPreagriculture <- function(cellular = FALSE, deposition = TRUE) {

  # calibrating the natural rate of leaching ####
  fixnat <- calcOutput("NitrogenFixationRateNatural", aggregate = FALSE)
  land   <- calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE)
  commonYears <- intersect(getYears(fixnat), getYears(land))
  fixnat <- fixnat[, commonYears, ]
  land <- land[, commonYears, ]
  fix    <- fixnat * land
  inputs <- (58 + 6 + 2.9 + 1.6 + 1.6 + 4)

  surplus <- fix / 58 * inputs
  # total natural inputs:
  # 58 fixation, (Vitousek 2013)
  # estimating global deposition
  # recylcing flows via air, assuming full recycling,disregarding ocean-land interaction
  # assuming proportional deposition to N fixation (not so unrealistic as most deposition should be close to emission)
  # 6 soil and veg Nh3, (Galloway 2004)
  # 2.9 NOX from natural soils (Galloway 2004)
  # 1.6+1.6 fire NOX+Nh3, (Galloway 2004)
  # 4 lightning, (Vitousek 2013)
  # assuming inputs=surplus given constant vegetation

  # leaching ####
  # 35 Tg of aquatic losses accoding to Vitousek 2013
  # check fire! it seems to be really big, see Braakhekke et al 2017
  # scale factors accordingly

  # avoiding division by zero
  surplus[surplus < 10^-10] <- 10^-10

  # to do: include EmisNitrogenShareNature
  # (Benni suggested to convert the previous warning to this comment)

  fracLeach <- calcOutput("IPCCfracLeach", aggregate = FALSE, cellular = TRUE)

  # accumulation in deserts
  deserts <- (fracLeach == 0)
  accumulationDeserts <- surplus * deserts
  surplusNonDeserts   <- surplus - accumulationDeserts
  inputsNonDesert     <- inputs * (1 - deserts)
  inputsNonDesert[inputsNonDesert == 0] <- 10^-10

  # gaseous losses ####
  nox <- (1.6 + 2.9) / inputsNonDesert * surplusNonDeserts
  nh3 <- (6 + 1.6) / inputsNonDesert * surplusNonDeserts
  # 6.8 Tg from Bouwman, A. F., Fung, I., Matthews, E. & John, J. Global analysis
  # of the potential for N2O production in natural soils. Global Biogeochemical
  # Cycles 7, 557–597 (1993).
  n2o <- (6.8) / inputsNonDesert * surplusNonDeserts

  # leaching losses
  leachingMultiplicationFactor <- setYears(35 / dimSums(surplus * fracLeach, dim = c(1, 3))[, "y1965", ], NULL)

  # surplus after gaseous losses
  surplusAfterLosses <- surplus - nox - nh3 - n2o
  no3                <- surplusAfterLosses * fracLeach * leachingMultiplicationFactor

  # n2
  n2 <- surplusAfterLosses - no3 - accumulationDeserts

  out <- mbind(
    add_dimension(no3, dim = 3.1, add = "form", nm = "no3_n"),
    add_dimension(nh3, dim = 3.1, add = "form", nm = "nh3_n"),
    add_dimension(nox, dim = 3.1, add = "form", nm = "no2_n"),
    add_dimension(n2o, dim = 3.1, add = "form", nm = "n2o_n_direct"),
    add_dimension(n2, dim = 3.1, add = "form", nm = "n2_n"),
    add_dimension(accumulationDeserts, dim = 3.1, add = "form", nm = "accumulation")
  )

  if (deposition == FALSE) {
    out <- out / inputs * 58 # only fixation
  }

  if (cellular == FALSE) {
    out <- dimSums(out, dim = c("x", "y"))
    out <- toolCountryFill(out, fill = colSums(out) * 10^-10)
  }

  return(list(x = out,
              weight = NULL,
              unit = "Mt Nr in various forms",
              min = 0,
              description = "Nitrogen emissions from soils under 100% natural cover (even for crop and urban)"))
}
