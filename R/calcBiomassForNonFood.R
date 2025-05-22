#' @title calcBiomassForNonFood
#' @description Calculates the supply of agricultural and forestry based biomass
#'              that are used in the energy or industry sector
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#'
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#' calcOutput("calcBiomassForNonFood")
#' }
calcBiomassForNonFood <- function() {
  # demands for
  # biofuel1stGen   - 1st generation bioenergy: oils, ethanol based
  # bioplastic      - feedstock: maiz, tece, potato, oils, sugar, betr, begr
  # traditionalFuel - burning for agricultural residues (res_cereals, res_fibrous, res_nonfibrous,),
  #                   and manure collected as fuel directls (not being confined at any point)
  # biogas          - only manure so far, forage crops or waste streams missing
  # woodBurning     - only woodfuel from forest sector
  # woodIndustrial  - only wood, including all industrial round wood usages
  # other_utils     - all other material demand (from FAO statistics)
  # Missing
  # - bagasse (from sugar production) and other co-products that are not tracked
  # - waste streams
  # - forest(ry) residues

  #### Load raw data from FAOmasslanace and create output object ####
  massbalance <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , c("bioenergy", "other_util")]
  attributes  <- calcOutput("Attributes", aggregate = FALSE)

  demandCategories <- c("biofuel1stGen", "bioplastic", "traditionalFuel", "woodBurning",
                        "woodIndustrial", "biogas", "other_util")
  kall             <- magpiesets::findset("kall")
  biomassResource  <- union(kall, c("manure", "res_wood"))
  kattr            <- getItems(attributes, dim = 3.1)
  kres             <- magpiesets::findset("kres")

  .combineDim <- function(x, y) {
    return(as.vector(outer(x, y, paste, sep = ".")))
  }
  tmp <- .combineDim(.combineDim(demandCategories, biomassResource), kattr)
  out <- new.magpie(names = tmp, years = getYears(massbalance), cells_and_regions = getItems(massbalance, dim = 1),
                    fill  = 0, sets = c("regions", "t", "demand.feedstock.attributes"))

  #### biofuel1stGen - 1st generation biofuels (just oils and ethanol) ####
  out[, , "biofuel1stGen"][, , kall] <- massbalance[, , "bioenergy"] # only ethanol and oils
  out[, , "biofuel1stGen"][, , kres] <- 0 # residues will be accounted in traditional burning

  #### bioplastics - feedstock: maiz, tece, potato, oils, sugar, betr, begr ####
  # Load required inputs
  plastic2biomass <- calcOutput("BioplasticToBiomass", aggregate = FALSE)
  demBioplastic   <- calcOutput("HistBioplasticProd", aggregate = FALSE) # Only 2010
  production      <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , "production"]

  # Calculate biomass demand for bioplastic production
  demandBiomassForPlastic <- plastic2biomass * demBioplastic * attributes[, , getNames(plastic2biomass)]
  bioplasticResources     <- getNames(demandBiomassForPlastic, dim = 1)

  # Identify available resources in massbalance
  massbalanceResources <- where(massbalance[, , "other_util", drop = TRUE][, , "dm", drop = TRUE] > 0)$true$data
  commonItems          <- intersect(bioplasticResources, massbalanceResources)
  missingItems         <- setdiff(bioplasticResources, commonItems)
  commonYears          <- intersect(getYears(demandBiomassForPlastic), getYears(out))

  # Function to calculate country weights (with fallback NA cleanup)
  .getCountryWeight <- function(baseData) {
    weight <- baseData / dimSums(baseData, dim = 1)
    toolConditionalReplace(weight, conditions = "is.na()", replaceby = 0)
  }

  # Fill output using massbalance-derived weights for common items
  if (length(commonItems) > 0) {
    countryWeight <- .getCountryWeight(massbalance[, , "other_util"])
    out[, commonYears, "bioplastic"][, , commonItems] <-
      demandBiomassForPlastic[, commonYears, commonItems] *
      countryWeight[, commonYears, "dm", drop = TRUE][, , commonItems]
  }

  # Fill output using production-derived weights for missing items
  if (length(missingItems) > 0) {
    countryWeight <- .getCountryWeight(production[, , missingItems])
    out[, commonYears, "bioplastic"][, , missingItems] <-
      demandBiomassForPlastic[, commonYears, missingItems] *
      countryWeight[, commonYears, "dm", drop = TRUE][, , missingItems]
  }

  # Calculate the remaining massbalance after subtracting bioplastic demand
  tmp <- massbalance[, , "other_util"][, , commonItems] - out[, , "bioplastic"][, , commonItems]

  # Check if bioplastic demand exceeds the available massbalance
  if (any(tmp < 0)) {
    print("Bioplastic demand exceeds massbalance, correcting Bioplastic demand.")
    # Create a correction matrix: keep only the negative part (i.e., the overuse)
    correction <- pmin(tmp, 0)
    # Reduce bioplastic demand by the overused amount
    out[, , "bioplastic"][, , commonItems] <-
      collapseDim(out[, , "bioplastic"][, , commonItems] + correction)
    # Subtract the corrected bioplastic demand from massbalance
    massbalance[, , "other_util"][, , commonItems] <-
      massbalance[, , "other_util"][, , commonItems] -
      out[, , "bioplastic"][, , commonItems]
  } else {
    # If all bioplastic demand is within limits, update massbalance directly
    massbalance[, , "other_util"][, , commonItems] <- tmp
  }

  #### traditionalFuel - burning for agricultural residues, manure directly collected from fields/pastures ####
  out[, , "traditionalFuel"][, , kres] <- massbalance[, , "bioenergy"][, , kres]

  excretion  <- calcOutput("Excretion", aggregate = FALSE, attributes = "npkc")
  manureFuel <- collapseDim(excretion[, , "fuel"])
  tmp   <- .combineDim(getNames(manureFuel, dim = 1), "dm")
  nr2dm <- new.magpie(names = tmp, years = NULL, cells_and_regions = "GLO",
                      fill  = 0, sets = c("regions", "t", "kli.attributes"))
  tmp    <- .combineDim(getNames(manureFuel, dim = 1), c("wm", "ge"))
  dm2wmGe <- new.magpie(names = tmp, years = NULL, cells_and_regions = "GLO",
                        fill  = 0, sets = c("regions", "t", "kli.attributes"))

  nr2dm[, , c("livst_chick", "livst_egg")] <- 1 / 0.051 # from IPCC guidelines nr to dm ratio
  nr2dm[, , "livst_milk"] <- 1 / 0.029 # from IPCC guidelines nr to dm ratio
  nr2dm[, , "livst_rum"]  <- 1 / 0.023 # from IPCC guidelines nr to dm ratio
  nr2dm[, , "livst_pig"]  <- 1 / 0.041 # from IPCC guidelines nr to dm ratio

  dm2wmGe[, , c("livst_chick", "livst_egg")][, , "ge"] <- 16 # simple search
  dm2wmGe[, , "livst_milk"][, , "ge"] <- 14 # simple search
  dm2wmGe[, , "livst_rum"][, , "ge"]  <- 16 # simple search
  dm2wmGe[, , "livst_pig"][, , "ge"]  <- 16 # simple search

  dm2wmGe[, , c("livst_chick", "livst_egg")][, , "wm"] <- 3 # simple search - super uncertian
  dm2wmGe[, , "livst_milk"][, , "wm"] <- 11 # simple search - super uncertian
  dm2wmGe[, , "livst_rum"][, , "wm"]  <- 10 # simple search - super uncertian
  dm2wmGe[, , "livst_pig"][, , "wm"]  <- 19 # simple search - super uncertian

  manureFuel <- mbind(manureFuel, manureFuel[, , "nr", drop = TRUE] * nr2dm)
  manureFuel <- mbind(manureFuel, manureFuel[, , "dm", drop = TRUE] * dm2wmGe)

  out[, , "traditionalFuel"][, , "manure"] <- dimSums(manureFuel, dim = 3.1)

  #### biogas - only manure in anaerobic digester, forage crop, waste as feedstock missing ####

  digester           <- excretion[, , "confinement"]
  animalWasteMSShare <- calcOutput("AWMSconfShrPast", aggregate = FALSE)[, , "digester"]
  manure2Biogas      <- collapseDim(digester * animalWasteMSShare)

  manure2Biogas <- mbind(manure2Biogas, manure2Biogas[, , "nr", drop = TRUE] * nr2dm)
  manure2Biogas <- mbind(manure2Biogas, manure2Biogas[, , "dm", drop = TRUE] * dm2wmGe)

  out[, , "biogas"][, , "manure"] <- dimSums(manure2Biogas, dim = 3.1)

  #### woodBurning - only woodfuel ####

  out[, , "woodBurning"][, , "woodfuel"] <- massbalance[, , "other_util"][, , "woodfuel"]
  massbalance[, , "other_util"][, , "woodfuel"] <- 0
  hhvWfuel   <- 20
  out[, , "woodBurning"][, , "woodfuel"][, , "ge"] <-
    out[, , "woodBurning"][, , "woodfuel"][, , "dm"] * hhvWfuel

  #### woodIndustrial - only industrial roundwood summed over all demand categories ####

  out[, , "woodIndustrial"][, , "wood"] <- massbalance[, , "other_util"][, , "wood"]
  massbalance[, , "other_util"][, , "wood"] <- 0
  hhvWfuel   <- 20
  out[, , "woodIndustrial"][, , "wood"][, , "ge"] <-
    out[, , "woodIndustrial"][, , "wood"][, , "dm"] * hhvWfuel

  #### other_utils - all other remaining material demand (from FAO statistics) ####

  out[, , "other_util"][, , kall] <- massbalance[, , "other_util"]

  #### END ####

  return(list(x = out,
              weight = NULL,
              unit = "Mt DM, Mt WM, PJ, Mt Nr, Mt C, Mt P, Mt K",
              description = "supply/demand of agricultural and forestry based biomass
                             that are used in the energy or industry sector"))
}
