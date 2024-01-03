#' @title calcSNUpE
#' @description calculates the soil nitrogen uptake efficiency. This is the nitrogen taken up from the soil
#' (N in crop biomass minus biological fixation minus seed N) divided by the soil N inputs
#' (fertilizer, manure etc). For the future, SNuPE scenarios are added.
#' @param max_snupe Maximum realistic SNUPE. All values above will be limited to this value. Only holds
#' for past values; future scneario values can exceed this number.
#' @param cellular disaggregated to 0.5 degree grid
#' @param rev revision number of madrat run
#' @param maccbase whether future scenarios should be expressed as base efficiency, excluding additional
#' macc improvemetns (new default)
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Xiaoxi Wang
#' @seealso
#' [calcNitrogenBudgetCropland()]
#' @examples
#' \dontrun{
#' calcOutput("SNUpE")
#' }
#'
calcSNUpE <- function(
    max_snupe = 0.85, # nolint: object_name_linter.
    cellular = FALSE, rev = 0.1, maccbase = TRUE) {

  a <- calcOutput("NitrogenBudgetCropland", max_snupe = max_snupe, aggregate = FALSE,
                  deposition = "Nsurplus2", cellular = cellular)

  a[, , "seed"] <- -a[, , "seed"]
  a[, , "fixation_crops"] <- -a[, , "fixation_crops"]
  a[, , "som"] <- a[, , "som"] * 1
  outputs <- c(
    "fixation_crops",
    "harvest", "ag", "bg", "seed")
  inputs <- c(
    "fixation_freeliving",
    "som", "fertilizer", "deposition",
    "manure_conf", "manure_stubble_grazing",
    "bg_recycling", "ag_recycling",
    "ag_ash", "balanceflow")
  outputs <- dimSums(a[, , outputs], dim = 3.1)
  inputs <- dimSums(a[, , inputs], dim = 3.1)
  sNUpE <- outputs / inputs
  sNUpE[is.na(sNUpE)] <- 0
  sNUpE[is.nan(sNUpE)] <- 0
  sNUpE[is.infinite(sNUpE)] <- 0
  sNUpE[sNUpE < 0] <- 0
  # future

  if (maccbase == FALSE) {

    zhang <- readSource("Zhang2015")
    data <- toolNUEscenarios(x = sNUpE, weight = inputs, rev = rev, zhang = zhang)
    weight <- data$weight
    out <- data$x

  } else {

    x <- setNames(toolHoldConstantBeyondEnd(sNUpE), "constant")
    weight <- setNames(toolHoldConstantBeyondEnd(inputs), NULL)

    # The first type of scenarios assume that NUE changes by the same number of percentage points
    # across regions. A maximum NUE however prevents too high NUEs.

    # function to incorporate CHA SNUPE due to fertilizer policy reform (Wang et al. 2023)
    wang2023Scenariosetting3 <- function(x, scenarioname, policy2020 = 0.122011, policy2050 = 0.192011,
                                         policy2100 = 0.292011, max = 100, adjust2050 = FALSE) {
      y2020CHN <- policy2020 + setYears(x["CHN", "y2010", "constant"], NULL)
      y2020CHN[y2020CHN > 70 / 100] <- 70 / 100
      x["CHN", , scenarioname] <- convergence(origin = x["CHN", , "constant"], aim = y2020CHN, start_year = "y2010",
                                              end_year = "y2020", type = "linear")
      x["CHN", 2015, scenarioname] <- x["CHN", 2015, scenarioname] - 0.0028612

      if (adjust2050) {
        y2050CHN <- policy2050 + setYears(x["CHN", "y2010", "constant"], NULL)
        y2050CHN[y2050CHN > max / 100] <- max / 100
        y2100CHN <- policy2100 + setYears(x["CHN", "y2010", "constant"], NULL)
        y2100CHN[y2100CHN > max / 100] <- max / 100
        x["CHN", , scenarioname] <- convergence(origin = x["CHN", , scenarioname], aim = y2050CHN, start_year = "y2020",
                                                end_year = "y2050", type = "linear")
        x["CHN", , scenarioname] <- convergence(origin = x["CHN", , scenarioname], aim = y2100CHN, start_year = "y2050",
                                                end_year = "y2100", type = "linear")
      }
      return(x)
    }

    scenariosetting3 <- function(x, y2020 = 0.03, y2050, y2100, max = 100) {
      scenarioname <- paste0("baseeff_add", y2020 * 100, "_add", y2050 * 100, "_add", y2100 * 100, "_max", max)
      x <- add_columns(x, addnm = scenarioname, dim = 3.1)
      y2020 <- y2020 + setYears(x[, "y2010", "constant"], NULL)
      y2020[y2020 > 70 / 100] <- 70 / 100
      y2050 <- y2050 + setYears(x[, "y2010", "constant"], NULL)
      y2050[y2050 > max / 100] <- max / 100
      y2100 <- y2100 + setYears(x[, "y2010", "constant"], NULL)
      y2100[y2100 > max / 100] <- max / 100
      x[, , scenarioname] <- convergence(origin = x[, , "constant"], aim = y2020, start_year = "y2010",
                                         end_year = "y2020", type = "linear")
      #incorporate changes in SNUPE due to fertilizer policy reform in China until 2020
      x <-  wang2023Scenariosetting3(x, scenarioname)

      x[, , scenarioname] <- convergence(origin = x[, , scenarioname], aim = y2050, start_year = "y2020",
                                         end_year = "y2050", type = "linear")
      x[, , scenarioname] <- convergence(origin = x[, , scenarioname], aim = y2100, start_year = "y2050",
                                         end_year = "y2100", type = "linear")

      return(x)
    }

    # Boundary
    #- most extremely optimistic scenario
    #- reaches about 90% efficiency in efficient countries with MACCs
    #- without mitigation, maximum levels stay close to current practices in best countries
    #- with mitiagion, maximum efficiency in 2100 can be achieved of 75% in current bad countries and
    #    up to 85-90% in current efficient countries
    x <- scenariosetting3(x, 0.03, 0.15, 0.25, max = 75)

    # Lowtech
    #- rapid improvement in less efficient countries,
    #- without mitigation, maximum levels stay close to current practices in best countries
    #- with mitiagion, maximum efficiency in 2100 can be achieved of 75% in current bad countries and
    #    up to 85-90% in current efficient countries
    x <- scenariosetting3(x, 0.03, 0.15, 0.25, max = 65)

    # SSP1
    #- rapid improvement in less efficient countries,
    #- without mitigation, maximum levels stay close to current practices in best countries
    #- with mitiagion, maximum efficiency in 2100 can be achieved of 75% in current bad countries and
    #    up to 85-90% in current efficient countries
    x <- scenariosetting3(x, 0.03, 0.10, 0.20, max = 75)

    # SSP2
    #- slight improvement in less efficient countries,
    #- without mitigation maximum efficiency in efficient countries drops
    #- with migigation, highes efficiency is around 80% in 2100
    x <- scenariosetting3(x, 0.03, 0.05, 0.10, max = 65)
    # SSP3
    #- no progress in inefficient countries,
    #- without mitigation, in efficient countries maximum efficiency drops
    #- with mitigation, up to 75% achievable in 2100
    x <- scenariosetting3(x, 0.03, 0, 0, max = 55)
    # SSP4
    #- neocolonialism and robotics allow for precision farming
    #- even more with market incentives
    #- still, efficient countries are able to make more progress
    x <- scenariosetting3(x, 0.03, 0.10, 0.15, max = 75)
    # SSP5
    #- rapid technological progress, but first effectivity than efficiency,
    #- so technical progress only kicks in in second half of century in
    #- low efficient economies
    x <- scenariosetting3(x, 0.03, 0.05, 0.15, max = 75)


    # This implementation assumes that the NUE target can only be achieved when the baseline NUE
    # and the maximum MACCs are combined
    # We combine the target NUE and the maximum MACCs potential to derive the baseline NUE.

    maxMaccs <- calcOutput("MACCsN2O", source = "PBL_MACC_2022", aggregate = FALSE)
    maxMaccs <- setNames(maxMaccs[, , "n2ofert.Default.201"], "maxmacc")[, c("y2050", "y2070", "y2100"), ]

    # NUE implicit to MACC curves
    # given that the maximum mitigaton is so similar across world regions, they likely
    # refer to an assumed uniform global NUE, which is likely close to 50 percent.
    implicitNue <- 0.5

    # MACCS that refer to input-dependent emission factors need to be transformed to become
    # NUE-MACCs.
    # See gams code for documentation of transformation of PBL curves
    maccsTransf <- maxMaccs * implicitNue / (1 + maxMaccs * (implicitNue - 1))


    # function to incorporate CHA SNUPE due to fertilizer policy reform (Wang et al. 2023)
    wang2023Scenariosetting2 <- function(x, scenarioname,  policyEffect = 0.122011) {

      y2020CHN <- policyEffect + x["CHN", "y2020", "constant"]
      y2020CHN[y2020CHN > 70] <- 70
      x["CHN", , scenarioname] <- convergence(origin = x["CHN", , "constant"], aim = y2020CHN, start_year = "y2010",
                                            end_year = "y2020", type = "linear")
      x["CHN", 2015, scenarioname] <- x["CHN", 2015, scenarioname] - 0.0028612
      return(x)
    }


    scenariosetting2 <- function(x, y2020, y2050, y2100) {
      # (1-NUE target) = (1-maccs transf)*(1-NUE_base)
      # NUE base = 1-(1-NUE target)/(1-maccs transf)
      scenarioname <- paste0("maxeff_add", y2020 * 100, "_glo", y2050 * 100, "_glo", y2100 * 100)
      x <- add_columns(x, addnm = scenarioname, dim = 3.1)
      y2020 <- y2020 + x[, "y2020", "constant"]
      y2020[y2020 > 70] <- 70
      y2050 <- 1 - (1 - y2050) / (1 - maccsTransf[, "y2050", ])
      y2100 <- 1 - (1 - y2100) / (1 - maccsTransf[, "y2100", ])

      x[, , scenarioname] <- convergence(origin = x[, , "constant"], aim = y2020, start_year = "y2010",
                                         end_year = "y2020", type = "linear")

      #incorporate changes in SNUPE due to fertilizer policy reform in China
      x <- wang2023Scenariosetting2(x, scenarioname)

      x[, , scenarioname] <- convergence(origin = x[, , scenarioname], aim = y2050, start_year = "y2020",
                                         end_year = "y2050", type = "linear")
      x[, , scenarioname] <- convergence(origin = x[, , scenarioname], aim = y2100, start_year = "y2050",
                                         end_year = "y2100", type = "linear")
      return(x)
    }

    # ssp2: not very globalized, constant mangement without policies

    # boundary: globalized, optimal management
    x <- scenariosetting2(x, 0.03, 0.75, 0.85)
    # ssp1: globalized, sustainable management (sustainable) (policy likely)
    # ssp4: globalized, good management (neo-colonialist robofarm) (policy not too likely)
    # ssp5: globalized, good management (hightech) (policy not too likely)
    x <- scenariosetting2(x, 0.03, 0.75, 0.8)
    # ssp3: deterioation without policies (policies unlikely)
    x <- scenariosetting2(x, 0.03, 0.6, 0.65)

    x <- scenariosetting2(x, 0.03, 0.65, 0.75)

    ### Zhang scenarios
    # maximum NUE from Zhang et al 2015
    zhang <- readSource("Zhang2015")

    scenarioname <- "maxeff_ZhangBy2030"
    x <- add_columns(x, addnm = scenarioname, dim = 3.1)
    y2020 <- 0.03
    y2020 <- y2020 + setYears(x[, "y2010", "constant"], NULL)
    y2020[y2020 > 70 / 100] <- 70 / 100
    y2030 <- 1 - (1 - zhang) / (1 - maccsTransf[, "y2050", ])
    y2070 <- 1 - (1 - 0.75) / (1 - maccsTransf[, "y2070", ])
    y2100 <- 1 - (1 - 0.8) / (1 - maccsTransf[, "y2100", ])
    x[, , scenarioname] <- convergence(origin = x[, , "constant"], aim = y2020, start_year = "y2010",
                                       end_year = "y2020", type = "linear")
    x[, , scenarioname] <- convergence(origin = x[, , scenarioname], aim = y2030, start_year = "y2020",
                                       end_year = "y2030", type = "linear")
    x[, , scenarioname] <- convergence(origin = x[, , scenarioname], aim = y2070, start_year = "y2030",
                                       end_year = "y2070", type = "linear")
    x[, , scenarioname] <- convergence(origin = x[, , scenarioname], aim = y2100, start_year = "y2070",
                                       end_year = "y2100", type = "linear")

    scenarioname <- "maxeff_ZhangBy2050"
    x <- add_columns(x, addnm = scenarioname, dim = 3.1)
    y2020 <- 0.03
    y2020 <- y2020 + setYears(x[, "y2010", "constant"], NULL)
    y2020[y2020 > 70 / 100] <- 70 / 100
    y2050 <- 1 - (1 - zhang) / (1 - maccsTransf[, "y2050", ])
    y2100 <- 1 - (1 - 0.8) / (1 - maccsTransf[, "y2100", ])
    x[, , scenarioname] <- convergence(origin = x[, , "constant"], aim = y2020, start_year = "y2010",
                                       end_year = "y2020", type = "linear")
    x[, , scenarioname] <- convergence(origin = x[, , scenarioname], aim = y2050, start_year = "y2020",
                                       end_year = "y2050", type = "linear")
    x[, , scenarioname] <- convergence(origin = x[, , scenarioname], aim = y2100, start_year = "y2050",
                                       end_year = "y2100", type = "linear")

    out <- x
  }

  return(list(
    x = out,
    weight = weight,
    unit = "Share",
    description = "Soil nitrogen uptake efficiency",
    isocountries = !cellular))
}
