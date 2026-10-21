#' @title calcAnimalStocks
#' @description calculates stocks of animals of different categories.
#'
#' @param grouping IPCC: Animal grouping of IPCC Guidelines
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Bin Lin
#' @seealso
#' [calcExcretionIPCC()],
#' [readIPCC()]
#' @examples
#' \dontrun{
#' calcOutput("AnimalStocks")
#' }
#'
calcAnimalStocks <- function(grouping = "IPCC") {
  if (grouping != "IPCC") {
    stop("so far only IPCC categories implemented.")
  }

  marketSwineShare <- 0.9 # table 10.19

  # converted here instead of in convertFAO_online() to keep NAs, so that toolFillStockGaps()
  # can tell gaps from real zeros
  fao <- readSource("FAO_online", "LiveHead2024", convert = "onlycorrect")
  relativeDelete <- c("Yield_(100_g/ha)", "Yield_Carcass_Weight_(Hg/An)", "Yield_(100mg/An)",
                      "Yield_(No/An)", "Yield_(100_mg/An)", "Yield_(100_g/An)",
                      "Yield_Carcass_Weight_(100_g/An)", "Yield_Carcass_Weight_(0_1_g/An)",
                      "Yield_(100_g)", "Yield_(kg/ha)", "Yield_Carcass_Weight_(g/An)",
                      "Yield_Carcass_Weight_(kg/An)")
  relativeDelete <- relativeDelete[relativeDelete %in% getItems(fao, dim = 3.2)]
  if (length(relativeDelete) > 0) {
    fao <- fao[, , relativeDelete, invert = TRUE]
  }

  prep <- toolFAOcountryPrep(fao)
  # NA weights are expected, as NAs are kept; they are set to 0 for the affected items only
  fao <- suppressSpecificWarnings(
    toolISOhistorical(prep$x, overwrite = TRUE, additional_mapping = prep$additionalMapping),
    "Weight in toolISOhistorical contained NAs", fixed = TRUE
  )
  fao <- toolCountryFill(fao, fill = NA, verbosity = 2)

  liveHead <- dimSums(fao, dim = "ElementShort")

  # gaps are filled per item before summing, so that a gap in one item is not masked by the others
  fillCombined <- function(stock, production) {
    filled <- stock
    for (i in seq_len(ndata(stock))) {
      filled[, , i] <- as.vector(toolFillStockGaps(stock[, , i], production[, , i]))
    }
    return(dimSums(filled, dim = 3))
  }

  # gap-filled species totals; the dairy/laying sub-populations are subtracted from them below
  cattleStock  <- toolFillStockGaps(liveHead[, , "866|Cattle"],
                                    fao[, , "867|Meat of cattle with the bone, fresh or chilled.Production_(t)"])
  buffaloStock <- toolFillStockGaps(liveHead[, , "946|Buffalo"],
                                    fao[, , "947|Meat of buffalo, fresh or chilled.Production_(t)"])
  sheepStock   <- toolFillStockGaps(liveHead[, , "976|Sheep"],
                                    fao[, , "977|Meat of sheep, fresh or chilled.Production_(t)"])
  goatStock    <- toolFillStockGaps(liveHead[, , "1016|Goats"],
                                    fao[, , "1017|Meat of goat, fresh or chilled.Production_(t)"])
  chickenHeadItems <- c("1057|Chickens", "1083|Other birds")
  chickenMeatItems <- c("1058|Meat of chickens, fresh or chilled.Production_(t)",
                        "1089|Meat of pigeons and other birds nec, fresh, chilled or frozen.Production_(t)")
  chickenStock <- fillCombined(liveHead[, , chickenHeadItems], fao[, , chickenMeatItems])
  # species used directly as IPCC categories, without a subtraction
  swineStock   <- toolFillStockGaps(liveHead[, , "1034|Swine / pigs"],
                                    fao[, , "1035|Meat of pig with the bone, fresh or chilled.Production_(t)"])
  horseStock   <- toolFillStockGaps(liveHead[, , "1096|Horses"],
                                    fao[, , "1097|Horse meat, fresh or chilled.Production_(t)"])
  duckStock    <- toolFillStockGaps(liveHead[, , "1068|Ducks"],
                                    fao[, , "1069|Meat of ducks, fresh or chilled.Production_(t)"])

  camelHeadItems <- c("1126|Camels", "1157|Other camelids")
  camelMeatItems <- c("1127|Meat of camels, fresh or chilled.Production_(t)",
                      "1158|Meat of other domestic camelids, fresh or chilled.Production_(t)")
  camelStock <- fillCombined(liveHead[, , camelHeadItems], fao[, , camelMeatItems])
  turkeyHeadItems <- c("1072|Geese", "1079|Turkeys")
  turkeyMeatItems <- c("1073|Meat of geese, fresh or chilled.Production_(t)",
                       "1080|Meat of turkeys, fresh or chilled.Production_(t)")
  turkeyStock <- fillCombined(liveHead[, , turkeyHeadItems], fao[, , turkeyMeatItems])
  muleAssHeadItems <- c("1107|Asses", "1110|Mules and hinnies")
  muleAssMeatItems <- c("1108|Meat of asses, fresh or chilled.Production_(t)",
                        "1111|Meat of mules, fresh or chilled.Production_(t)")
  muleAssStock <- fillCombined(liveHead[, , muleAssHeadItems], fao[, , muleAssMeatItems])

  # dairy/laying sub-population series (used below to split each species' total stock
  # into dairy-or-layer vs other/broiler), gap-filled against milk/egg production rather
  # than meat production. Raw (pre-fill) copies are kept for toolImputeSubShare() below.
  dairyCowsRaw    <- fao[, , "882|Raw milk of cattle.Milk_Animals_(An)"]
  dairyCowsStock  <- toolFillStockGaps(dairyCowsRaw, fao[, , "882|Raw milk of cattle.Production_(t)"])
  dairyBufRaw     <- fao[, , "951|Raw milk of buffalo.Milk_Animals_(An)"]
  dairyBufStock   <- toolFillStockGaps(dairyBufRaw, fao[, , "951|Raw milk of buffalo.Production_(t)"])
  dairySheepRaw   <- fao[, , "982|Raw milk of sheep.Milk_Animals_(An)"]
  dairySheepStock <- toolFillStockGaps(dairySheepRaw, fao[, , "982|Raw milk of sheep.Production_(t)"])
  dairyGoatRaw    <- fao[, , "1020|Raw milk of goats.Milk_Animals_(An)"]
  dairyGoatStock  <- toolFillStockGaps(dairyGoatRaw, fao[, , "1020|Raw milk of goats.Production_(t)"])
  layerHeadItems <- c("1062|Hen eggs in shell, fresh.Laying_(An)",
                      "1091|Eggs from other birds in shell, fresh, nec.Laying_(An)")
  layerProdItems <- c("1062|Hen eggs in shell, fresh.Production_(t)",
                      "1091|Eggs from other birds in shell, fresh, nec.Production_(t)")
  layerRaw        <- toolCombineItems(fao, layerHeadItems, dim = 3)
  layerStock      <- fillCombined(fao[, , layerHeadItems], fao[, , layerProdItems])
  dairyCamelRaw   <- fao[, , "1130|Raw milk of camel.Milk_Animals_(An)"]
  dairyCamelStock <- toolFillStockGaps(dairyCamelRaw, fao[, , "1130|Raw milk of camel.Production_(t)"])

  # see toolImputeSubShare() header
  dairyCowsStock  <- toolImputeSubShare(dairyCowsStock, cattleStock, dairyCowsRaw)
  dairyBufStock   <- toolImputeSubShare(dairyBufStock, buffaloStock, dairyBufRaw)
  dairySheepStock <- toolImputeSubShare(dairySheepStock, sheepStock, dairySheepRaw)
  dairyGoatStock  <- toolImputeSubShare(dairyGoatStock, goatStock, dairyGoatRaw)
  layerStock      <- toolImputeSubShare(layerStock, chickenStock, layerRaw)
  dairyCamelStock <- toolImputeSubShare(dairyCamelStock, camelStock, dairyCamelRaw)

  # Externally-sourced corrections for country/category gaps toolFillStockGaps() leaves at 0
  # (no in-dataset production evidence), using directly comparable national-statistics figures.
  # Narrow by design: only where a specific, cross-checked number was found, only for years
  # still at 0, never overwriting FAO-resolved years; ambiguous cases are left at 0.
  applyExternalFill <- function(x, iso, years, value) {
    yrCols <- paste0("y", years)
    cur <- x[iso, yrCols, ]
    stillUnresolved <- as.numeric(cur) == 0
    cur[stillUnresolved] <- value
    x[iso, yrCols, ] <- cur
    x
  }

  # poultry layers (DPo)
  # Bulgaria Ministry of Agriculture and Food, 2024
  layerStock <- applyExternalFill(layerStock, "BGR", 2021:2024, 4392000)
  layerStock <- applyExternalFill(layerStock, "GLP", 2007:2024, 220155)  # DAAF Guadeloupe RA2020, 2020
  layerStock <- applyExternalFill(layerStock, "GUF", 2007:2024, 72000)   # DAAF Guyane Memento 2017, 2016
  # Eurostat egg-laying-hen statistics, 2022 (likely a slight
  # underestimate given IE's growing production trend since)
  layerStock <- applyExternalFill(layerStock, "IRL", 2021:2024, 3100000)
  # last FAO value (2006) held constant - low confidence,
  # Agreste Martinique census notes an ongoing decline
  layerStock <- applyExternalFill(layerStock, "MTQ", 2007:2024, 95000)
  # last FAO value (2006) held constant - medium confidence
  layerStock <- applyExternalFill(layerStock, "REU", 2007:2024, 508000)

  # ducks (Dk) - main species total, not a dairy/laying sub-population
  # DAAF Guyane Memento 2017, 2015 (700 canards a gaver +
  # 4,000 canards a rotir); last FAO value (30,000) was ~6x too high
  duckStock <- applyExternalFill(duckStock, "GUF", 2007:2024, 4700)
  duckStock <- applyExternalFill(duckStock, "IRL", 2021:2024, 434000) # last FAO value (2020) held constant
  # last FAO value (2001) held constant - low confidence, decades old
  duckStock <- applyExternalFill(duckStock, "MTQ", 2002:2024, 590000)
  duckStock <- applyExternalFill(duckStock, "REU", 2007:2024, 520000) # last FAO value (2006) held constant

  # dairy cows (DCt)
  dairyCowsStock <- applyExternalFill(dairyCowsStock, "MTQ", 2007:2024, 96)    # DAAF Martinique Memento 2016, 2015;
  # last FAO value (2,565, 2006) was ~27x too high
  dairyCowsStock <- applyExternalFill(dairyCowsStock, "REU", 2007:2024, 27280) # last FAO value (2006) held constant

  # dairy buffalo (DBf)
  # last FAO value (2019) held constant; negligibly small
  dairyBufStock <- applyExternalFill(dairyBufStock, "ALB", 2020:2024, 29)
  # Philippine Statistics Authority Dairy Situation Report,
  # 2023 - real growth since the 1990 baseline (8,000), so this
  # flat fill understates the 1990s-2000s and likely
  # overstates the 1991-1992 transition years slightly
  dairyBufStock <- applyExternalFill(dairyBufStock, "PHL", 1991:2024, 30151)

  dairyCowsStock  <- toolClampToTotal(dairyCowsStock,  cattleStock,  "dairy cows")
  dairyBufStock   <- toolClampToTotal(dairyBufStock,   buffaloStock, "dairy buffalo")
  dairySheepStock <- toolClampToTotal(dairySheepStock, sheepStock,   "dairy sheep")
  dairyGoatStock  <- toolClampToTotal(dairyGoatStock,  goatStock,    "dairy goats")
  layerStock      <- toolClampToTotal(layerStock,      chickenStock, "poultry layers")
  dairyCamelStock <- toolClampToTotal(dairyCamelStock, camelStock,   "dairy camels")

  # estimate numbers of animals for IPCC categories
  animals <- NULL

  # Dairy cows
  animals <- mbind(animals, setNames(
                                     collapseNames(dairyCowsStock), "dairy cows"))
  # Other cattle
  animals <- mbind(animals, setNames(
    cattleStock
    - setNames(animals[, , "dairy cows"], NULL),
    "other cattle"
  ))

  # Dairy Buffalo
  animals <- mbind(animals, setNames(
                                     collapseNames(dairyBufStock), "dairy buffalo"))
  # Other buffalo
  animals <- mbind(animals, setNames(
                                     buffaloStock
                                     - setNames(animals[, , "dairy buffalo"], NULL), "other buffalo"))

  # Market Swine
  animals <- mbind(animals, setNames(
    swineStock * marketSwineShare,
    "market swine"
  ))

  # Breeding Swine
  animals <- mbind(animals, setNames(
    swineStock * (1 - marketSwineShare),
    "breeding swine"
  ))

  # Dairy Sheep
  animals <- mbind(animals, setNames(
                                     collapseNames(dairySheepStock), "dairy sheep"))
  # Other sheep
  animals <- mbind(animals, setNames(
    sheepStock
    - setNames(animals[, , "dairy sheep"], NULL),
    "other sheep"
  ))

  # Dairy Goats
  animals <- mbind(animals, setNames(
                                     collapseNames(dairyGoatStock), "dairy goats"))
  # Other goats
  animals <- mbind(animals, setNames(
    goatStock
    - setNames(animals[, , "dairy goats"], NULL),
    "other goats"
  ))

  # Dairy Camels
  animals <- mbind(animals, setNames(
                                     collapseNames(dairyCamelStock), "dairy camels"))
  # Other Camelids
  animals <- mbind(animals, setNames(
    camelStock
    - setNames(animals[, , "dairy camels"], NULL),
    "other camels"
  ))

  # Horses
  animals <- mbind(animals, setNames(
    horseStock,
    "horses"
  ))

  # Mules and Asses
  animals <- mbind(animals, setNames(
    muleAssStock,
    "mules and asses"
  ))

  # Poultry Layers
  animals <- mbind(animals, setNames(
                                     collapseNames(layerStock),
                                     "poultry layers"))
  # Broilers
  animals <- mbind(animals, setNames(
    chickenStock
    - setNames(animals[, , "poultry layers"], NULL),
    "broilers"
  ))

  # Turkey
  animals <- mbind(animals, setNames(
    turkeyStock,
    "turkey"
  ))

  # Ducks
  animals <- mbind(animals, setNames(
    duckStock,
    "ducks"
  ))

  # ignore
  # "1140|Rabbits and hares","1150|Rodents, other"
  animals <- animals / 1000000

  # sort according to n_rate animal categories

  # any NA remaining here is a country FAO never tracked for that item at all (dropped by
  # toolCountryFill(fill = NA) at read-in), not a within-series gap - toolFillStockGaps()
  # and toolImputeSubShare() already handled those. Cleaned up only here because the steps
  # above need the NA/real-zero distinction.
  animals[is.na(animals)] <- 0

  # remove all negative values
  remove <- which(animals < 0)

  if (length(remove) > 0) {
    vcat(2, paste0(length(remove), " negative values removed"))
    animals[remove] <- 0
  }

  return(list(
    x = animals,
    weight = NULL,
    unit = "Million animals",
    description = "Animal stocks, for laying hens and dairy cattle producing animals",
    min = 0
  ))
}
