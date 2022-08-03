#' @title calcBioplsticToBiomass
#' @description calculates conversion factors from bioplastic demand to demand of biomass needed for the
#' production, taking into account the average share of different biomass sources (glycerol, starch, sugars, cellulose,
#' oils) for bioplastic production and corresponding content in the different crop types
#' @return List of magpie objects with global conversion factors, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("BioplsticToBiomass")
#' }

calcBioplsticToBiomass <- function() {

  ## overall biomass demand for bioplastics
  # conversion from bioplastic demand to overall biomass demand
  # Source: Bio-based Building Blocks and Polymers – Global Capacities, Production and Trends 2020 – 2025 (short
  # version), nova-Institute, 2021
  bioplastic2biomass       <- 4.8 / 4

  # distribution to different biomass sources
  # Source: Bio-based Building Blocks and Polymers – Global Capacities, Production and Trends 2020 – 2025 (short
  # version), nova-Institute, 2021
  sharesBiomassDemand <- data.frame("biomass" = c("glycerol", "starch", "sugars", "non-edible plant oils",
                                                  "cellulose", "edible plant oils"),
                                    "share"   =  c(0.37, 0.24, 0.16, 0.12, 0.09, 0.02))
  sharesBiomassDemand <- as.magpie(sharesBiomassDemand)

  # biomass demand
  biomassDemand <- sharesBiomassDemand * bioplastic2biomass


  ## Map to MAgPIE products
  # -> sugars already included in MAgPIE as processed category
  # -> edible and non-edible oils can both be mapped to oils in MAgPIE
  # -> cellulose mapped to begr and betr with 50/50 shares
  # -> starch mapped to potato, maize, wheat (-> tece) in equal parts
  # -> glycerol: byproduct from biodiesel production, but not inculded as byproduct in MAgPIE,
  #              would be difficult to implement => ignore for now

  cropProducstDemand <- new.magpie("GLO", years = getYears(biomassDemand),
                                   names = c("sugar", "oils", "begr", "betr", "potato", "maiz", "tece"),
                                   sets = c("region", "year", "source"), fill = 0)

  # products already in MAgPIE
  cropProducstDemand[, , "sugar"] <- biomassDemand[, , "sugars"]
  cropProducstDemand[, , "oils"]  <- biomassDemand[, , "non-edible plant oils"] + biomassDemand[, , "edible plant oils"]

  # cellulose (split 50/50 between begr and betr)
  # sources: Sannigrahi et al, 2010, Poplar as a feedstock for biofuels: A review of compositional characteristics
  #          Amarasekara, 2013, Handbook of Cellulosic Ethanol, Chapter 3 (Feedstocks for Cellulosic Ethanol Production)
  #          Ribeiro et al, 2018, EFFECT OF RESIDUAL EFFECTIVE ALKALI ON EUCALYPTUS KRAFT PULP YIELD AND CHEMISTRY
  celluloseContentPoplar <- data.frame("type" = c("P. deltoides", "NM6", "CAFI low lignin", "CAFI high lignin",
                                                  "Caudina DN 34", "DN 182", "DN 17", "NC 5260"),
                                       "share" = c(42.2, 48.95, 43.8, 45.1, 43.67, 45.52, 43.65, 45.08) / 100)
  celluloseContentWillow <- data.frame("type" = "willow",
                                       "share" = c(43.83) / 100)
  celluloseContentEucalyptus <- data.frame("type" = c("E. nitens x E globulus hybrid", "E. nitens", "E. globulus",
                                                      "E. urophylla x E. grandis hybrid"),
                                           "share" = c(46.8, 47.6, 51.0, 51.3) / 100)
  celluloseContentMiscanthus <- data.frame("age" = c(1, 2, 3, 4, 5),
                                           "share" = c(41.7, 44.4, 47.8, 53.1, 53.6) / 100)
  celluloseContentBetr <- (mean(celluloseContentPoplar$share) + mean(celluloseContentWillow$share) +
                             mean(celluloseContentEucalyptus$share)) / 3
  celluloseContentBegr <- mean(celluloseContentMiscanthus$share)

  cropProducstDemand[, , "begr"] <- biomassDemand[, , "cellulose"] * 0.5 * (1 / celluloseContentBegr)
  cropProducstDemand[, , "betr"] <- biomassDemand[, , "cellulose"] * 0.5 * (1 / celluloseContentBetr)

  # starch
  # source: IfBB – Institute for Bioplastics and Biocomposites, 2021, Biopolymers facts and statistics 2021 –
  #         Production capacities, processing routes, feedstock, land and water use, Hochschule Hannover
  # given as share of wet matter
  starchContentPota  <- 0.18   # potato tuber
  starchContentMaiz  <- 0.7    # maize kernel
  starchContentWheat <- 0.46   # wheat grains
  wm2dm <- 1 / calcOutput("Attributes", aggregate = FALSE)[, , list("wm", c("potato", "maiz", "tece"))]
  cropProducstDemand[, , "potato"] <- biomassDemand[, , "starch"] * 1/3 * (1 / starchContentPota) * wm2dm[, , "potato"]
  cropProducstDemand[, , "maiz"]   <- biomassDemand[, , "starch"] * 1/3 * (1 / starchContentMaiz) * wm2dm[, , "maiz"]
  cropProducstDemand[, , "tece"]   <- biomassDemand[, , "starch"] * 1/3 * (1 / starchContentWheat) * wm2dm[, , "tece"]

  return(list(x = cropProducstDemand,
              weight = NULL,
              unit = "mio. t dm",
              description = "Demand of crop products for one tonne of bioplastic production"))
}
