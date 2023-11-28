#' @title calcFAOmassbalance_pre
#' @description Calculates an extended version of the Food Balance Sheets. Makes explicit the conversion processes that
#' convert one type of product into another. Includes processes like milling, distilling, extraction etc. Adds certain
#'  byproducts like distillers grains or ethanol.
#'
#' @param years years to be estimated, if null, then all years in FAOharmonized are returned
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' This is an intermediary result, which is used e.g. for estimating the feed baskets. For most uses, it is more
#' appropriate to use the FAOmasbalance instead of the FAOmassbalance_pre.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcFAOmassbalance()]
#' @examples
#' \dontrun{
#' calcOutput("FAOmassbalance_pre")
#' }
#' @importFrom graphics plot
#' @importFrom magclass getSets as.magpie complete_magpie
#' @importFrom utils read.csv
#' @importFrom withr local_options

calcFAOmassbalance_pre <- function(years = NULL) { # nolint
  #### Data input ####

  ### FAO Commodity Balance
  cbc          <- calcOutput(type = "FAOharmonized", aggregate = FALSE)
  getSets(cbc) <- c("region", "year", "ItemCodeItem.ElementShort")

  if (any(duplicated(dimnames(cbc)[[3]]) == TRUE)) {
    stop("The folowing dimnames are duplicated: ", paste(getNames(cbc)[duplicated(getNames(cbc))], collapse = "\", \""))
  }

  # determine years
  if (is.null(years)) {
    years <- getYears(cbc)
  }
  if (nchar(years[[1]]) < 5) {
    years <- paste0("y", years)
  }

  cbc <- cbc[, years, ]

  # remove double counting and add missing products
  removethem <- c(
    # crop commodity balance and Food Supply items aggregated
    "2924|Alcoholic Beverages",
    "2905|Cereals - Excluding Beer",
    "2919|Fruits - Excluding Wine",
    "2928|Miscellaneous",
    "2913|Oilcrops",
    "2911|Pulses",
    "2923|Spices",
    "2907|Starchy Roots",
    "2922|Stimulants",
    "2909|Sugar & Sweeteners",
    "2908|Sugar Crops",
    "2912|Treenuts",
    "2914|Vegetable Oils",
    "2918|Vegetables",
    "2903|Vegetal Products",
    "2901|Grand Total",
    # livestock commodity balance and Food Supply items aggregated
    "2941|Animal Products",
    "2946|Animal fats",
    "2961|Aquatic Products, Other",
    "2949|Eggs",
    "2960|Fish, Seafood",
    "2943|Meat",
    "2948|Milk - Excluding Butter",
    "2738|Milk, Whole",
    "2739|Milk, Skimmed",
    "2945|Offals",
    # others and equivalents
    "2741|Cheese",
    "2556|Groundnuts (Shelled Eq)",
    "2562|Palm kernels",
    "2805|Rice (Milled Equivalent)",
    "2815|Roots & Tuber Dry Equiv",
    "2672|Rubber",
    "2747|Silk",
    "2827|Sugar, Raw Equivalent",
    "2542|Sugar (Raw Equivalent)",
    "2671|Tobacco",
    "2742|Whey"
  )

  cbc <- cbc[, , removethem, invert = TRUE]

  cbc <- complete_magpie(cbc, fill = 0)

  missingproducts <- c("X001|Ethanol",
                       "X002|Distillers_grain",
                       "X003|Palmoil_Kerneloil_Kernelcake",
                       "X004|Brewers_grain")

  cbc <- add_columns(cbc, addnm = missingproducts, dim = 3.1)


  #### Product Attributes
  prodAttributes <- calcOutput("Attributes", aggregate = FALSE)
  attributeTypes <- getNames(prodAttributes, dim  = 1)
  removeProd     <- c("betr", "begr", "pasture", "scp", "res_cereals",
                      "res_fibrous", "res_nonfibrous", "wood", "woodfuel")
  prodAttributes <- prodAttributes[, , removeProd, invert = TRUE]

  # Sectoral mapping for FAO items
  relationmatrix <- toolGetMapping("FAOitems_online.csv", type = "sectoral", where = "mappingfolder")
  relationmatrix <- relationmatrix[, c("FoodBalanceItem", "k")]
  relationmatrix <- relationmatrix[!duplicated(relationmatrix[, "FoodBalanceItem"]), ]

  .getFAOitems <- function(magpieItems) {
    return(relationmatrix[relationmatrix$k %in% magpieItems, "FoodBalanceItem"])
  }

  # Map production attributes to FAO items
  prodAttributes          <- toolAggregate(x = prodAttributes, rel = relationmatrix, dim = 3.2, from = "k",
                                           to = "FoodBalanceItem", partrel = TRUE)
  getSets(prodAttributes) <- c("region", "year", "attributes", "ItemCodeItem")

  # change prod attributes from share of dm to share of wm
  attributesWM <- (prodAttributes / dimSums(prodAttributes[, , "wm"], dim = "attributes"))

  cbcItemnames <- getNames(cbc, dim = "ItemCodeItem")
  itemnamesAttributes <- getNames(attributesWM, dim = "ItemCodeItem")
  if (!(all(cbcItemnames %in% itemnamesAttributes))) {
    vcat(verbosity = 2, "The following items were removed from the dataset because of missing prodAttributes: ",
         paste(cbcItemnames[!(cbcItemnames %in% itemnamesAttributes)], collapse = "\", \""))
    cbc <- cbc[, , cbcItemnames[cbcItemnames %in% itemnamesAttributes]]
    relationmatrix <- relationmatrix[relationmatrix[, "FoodBalanceItem"] %in%
                                       intersect(cbcItemnames, itemnamesAttributes), ]
  }
  if (!all(itemnamesAttributes %in% cbcItemnames)) {
    stop("For the following items there were entries in prodAttributes but no respective data: ",
         paste(itemnamesAttributes[!(itemnamesAttributes %in% cbcItemnames)], collapse = "\", \""))
  }


  ### FAO items not relevant to processing, and processing dimensions that need to be added
  noProcessing <- c("livst_rum", "livst_pig", "livst_milk", "livst_egg", "livst_chick", "foddr", "fish", "fibres")
  noProcessingFAO <- .getFAOitems(noProcessing)
  noProcessingFAO <- noProcessingFAO[noProcessingFAO %in% getNames(cbc, dim = 1)]

  namesProcessing <- c("production_estimated",
                       "milling", "brans1", "branoil1", "flour1",
                       "refining", "sugar1", "molasses1", "refiningloss",
                       "extracting", "oil1", "oil2", "oilcakes1", "extractionloss",
                       "fermentation", "alcohol1", "alcohol2", "alcohol3", "alcohol4", "brewers_grain1", "alcoholloss",
                       "distilling", "ethanol1", "distillers_grain1", "distillingloss",
                       "intermediate",
                       "households")

  #### Definition of subfunctions #####

  # run massbalance checks and clear processed positions after calculating process
  .checkAndClear <- function(object,
                             goodsIn,
                             from,
                             process,
                             reportAs,
                             residual,
                             relevantAttributes = attributeTypes,
                             goodsOut = NULL,
                             threshold = 1e-5) {
    # perform massbalance tests:
    # 1) input goods balanced?
    diff <- (dimSums(object[, , list(goodsIn, c(reportAs, residual))], dim = c("ElementShort"))
             - dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort")))
    if (any(abs(diff) > threshold)) {
      stop("NAs in dataset or function corrupt: process not balanced for ",
           paste(goodsIn, collapse = ", "), " reported as ", paste(reportAs, collapse = ", "))
    }

    # 2) output goods balanced...
    if (!is.null(goodsOut)) {
      # ... with input goods?
      diff <- (dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort", "ItemCodeItem"))
        - dimSums(object[, , list(goodsIn, residual)], dim = c("ElementShort", "ItemCodeItem"))
        - dimSums(object[, , list(goodsOut, "production_estimated")], dim = c("ElementShort", "ItemCodeItem"))
      )
      if (any(abs(diff) > threshold)) {
        stop("NAs in dataset or function corrupt: goods not balanced for ",
             paste(goodsOut, collapse = ", "), " from ", paste(goodsIn, collapse = ", "))
      }

      # ... in production?
      diff <- (sum(object[, , list(goodsOut, "production_estimated")])
               - sum(object[, , list(goodsOut, "production")]))
      if (any(abs(diff) > threshold)) {
        stop("Global estimated production does not meet global production for ",
             paste(goodsOut, collapse = ", "))
      }
    }

    # 4) special case for cereal milling: branoils balanced?
    if (residual == "flour1") {
      diff <- (dimSums(object[, , list(goodsIn, "branoil1")], dim = c("ElementShort", "ItemCodeItem"))
               - dimSums(object[, , list(c("2581|Ricebran Oil", "2582|Maize Germ Oil"), "production_estimated")],
                         dim = c("ElementShort", "ItemCodeItem")))
      if (any(abs(diff) > threshold)) {
        stop("NAs in dataset or function corrupt: branoil1 not balanced")
      }
    }

    # negative value check
    relValues <- object[, , list(goodsIn, c(reportAs, from, residual), relevantAttributes)]
    if (any(relValues < -threshold)) {
      warning("Massbalancing failed, negative values for ",
              paste(unique(unname(where(relValues < -threshold)[[1]]$individual[, 3])), collapse = ", "))
    }

    # move from "from" to "process" and clear "from"
    if (from != process) {
      object[, , list(goodsIn, process)] <- object[, , list(goodsIn, from)]
    }
    object[, , list(goodsIn, from)] <- 0  # if from == process it is "intermediate" which is to be cleared as well

    gc()
    return(object)
  }

  # Different processes, e.g. ethanol production from cereals, are not specified
  # in cbc (instead the general categories "other_util" and "processed" are used),
  # but are required within MAgPIE.
  # This function calculates the maximum amount out of a given product "goodIn.from"
  # that can be used to produce a given output product "goodOut", depending on
  # the "extractionQuantity" and the "extractionAttribute". This quantity is
  # reported as "goodIn.reportAs", while any remaining quantity in "goodIn.from"
  # is reported as "goodIn.residual". The full amount in "goodIn.from" is then
  # moved to "goodIn.process" (i.e. "goodIn.from" will be empty after the function
  # call). The variable "process" specifies the process leading to the output product
  # "reportAs".
  # The calculated production quantity is also added to "goodOut.production_estimated".
  .extractGoodFromFlow <- function(object,
                                   goodIn,              # FAO-defined input product, e.g. "2536|Sugar cane"
                                   from,                # FAO-defined process, e.g. "other_util"
                                   process,             # MAgPIE-defined process, e.g. "distilling"
                                   goodOut,             # FAO-defined output product, e.g. "X001|Ethanol"
                                   reportAs,            # MAgPIE-defined output product, e.g. "ethanol1"
                                   residual,            # MAgPIE-defined residual, e.g. "distillingloss"
                                   extractionQuantity,  # e.g. 0.516006
                                   extractionAttribute, # e.g. "dm"
                                   prodAttributes) {

    if (length(from) > 1 || length(reportAs) > 1 || length(goodIn) > 1 || length(goodOut) > 1) {
      stop("please only use one item each for \"from\", \"reportAs\", \"goodIn\", and \"goodOut\"")
    }
    if (any(object[, , list(goodIn, c(reportAs, residual))] != 0)) {
      warning("Output flows already exist!")
    }

    # relevant attributes for extraction quantity
    attrNoWM <- setdiff(attributeTypes, "wm")

    # calculating possible extraction quantity per attribute
    attributesFrom   <- dimSums(object[, , list(goodIn, from), drop = TRUE], dim = "region") /
      dimSums(object[, , list(goodIn, from, extractionAttribute), drop = TRUE], dim = c("region"))
    attributesTo     <- prodAttributes[, , goodOut, drop = TRUE] /
      prodAttributes[, , list(goodOut, extractionAttribute), drop = TRUE]
    extractionFactor <- attributesFrom[, , attrNoWM] / attributesTo[, , attrNoWM]

    # maximum extraction quantity as minimum over the possible quantity per attribute
    maxextract <- as.magpie(apply(X = extractionFactor, MARGIN = 2, FUN = min))
    if (extractionQuantity == "max") {
      extractionQuantity <- maxextract
    } else if (any(extractionQuantity > maxextract)) {
      stop("too high extraction quantity")
    }

    # calculate outputs
    extracted <- object[, , list(goodIn, from, extractionAttribute), drop = TRUE] * extractionQuantity * attributesTo
    losses    <- dimSums(object[, , list(goodIn, from)], dim = "ElementShort") - extracted

    object[, , list(goodIn, reportAs)] <- extracted
    object[, , list(goodIn, residual)] <- losses

    object[, , list(goodOut, "production_estimated")] <- object[, , list(goodOut, "production_estimated")] + extracted

    # check results and clear processed position
    object <- .checkAndClear(object, goodIn, from, process, reportAs, residual, attrNoWM)

    return(object)
  }

  # This function is similar to .extractGoodFromFlow, with the difference that
  # multiple input goods can be given (which will then be added up before calculating
  # the amount of "goodsOut" that can be produced), and that multiple output goods
  # (and corresponding items in reportAs) can be given. The order of FAO categories
  # in "goodsOut" and corresponding MAgPIE categories in "reportAs" needs to match!
  # In contrast to .extractGoodFromFlow, this function calculates global
  # conversion factors per attribute instead of using an "extractionQuantity"
  # and "extractionAttribute" for calculations.
  .processingGlobal <- function(object,
    goodsIn,  # e.g. c("2536|Sugar cane", "2537|Sugar beet")
                                from,     # e.g. "processed" #nolint
                                process,  # e.g. "refining"
                                goodsOut, # e.g. c("2818|Sugar, Refined Equiv", "2544|Molasses") - (the order matters!)
                                reportAs, # e.g. c("sugar1", "molasses1") - (the order matters!)
                                residual  # e.g. "refiningloss"
  ) {
    if (any(object[, , list(goodsIn, c(reportAs, residual))] != 0)) {
      stop("Output flows already exist.")
    }
    if (any(object[, , list(goodsOut, "production_estimated")] != 0)) {
      stop("Output flows already exist.")
    }

    # attributes relevant for checking massbalance and convFactor
    relevantAttributes <- setdiff(attributeTypes, "wm")

    # calculate global conversion factor per attributes
    convFactor <- (dimSums(object[, , list(goodsOut, "production")], dim = c("region", "ElementShort"))
                   / dimSums(object[, , list(goodsIn, from)], dim = c("region", "ItemCodeItem", "ElementShort")))

    factors <- dimSums(convFactor[, , list(goodsOut, relevantAttributes)], dim = "ItemCodeItem")
    if (any(factors > 1)) {
      stop("conversion factors exceed 1. not suitable for a global conversion factor.",
           paste(unique(unname(where(factors > 1)[[1]]$individual)), collapse = ", "))
    }

    # estimate outputs
    for (j in seq_along(goodsOut)) {
      object[, , list(goodsIn, reportAs[j])] <- dimSums(object[, , list(goodsIn, from)], dim = "ElementShort") *
        convFactor[, , goodsOut[j], drop = TRUE]
      object[, , list(goodsOut[j], "production_estimated")] <- dimSums(object[, , list(goodsIn, reportAs[j])],
                                                                       dim = c("ElementShort", "ItemCodeItem"))
    }

    # calculate refining losses as mass balance difference
    object[, , list(goodsIn, residual)] <- (dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort"))
                                            - dimSums(object[, , list(goodsIn, reportAs)], dim = c("ElementShort")))

    # check results and clear processed position
    object <- .checkAndClear(object, goodsIn, from, process, reportAs, residual, relevantAttributes, goodsOut)

    return(object)
  }


  # processing of cereals (milled) to bran and flour: this is the only process that does
  # not use the functions .extractGoodFromFlow and/or .processingGlobal,
  # as the extraction quantity of bran is calculated in a specific way, using the
  # ratio of bran to full cereal as given by Feedipedia.
  .cerealMillingGlobal <- function(object) {

    cereals <- c("2511|Wheat and products",
                 "2513|Barley and products",
                 "2514|Maize and products",
                 "2515|Rye and products",
                 "2516|Oats",
                 "2517|Millet and products",
                 "2518|Sorghum and products",
                 "2520|Cereals, Other",
                 "2804|Rice (Paddy Equivalent)")

    brans   <- .getFAOitems("brans")
    milled  <- "food"
    flour   <- "flour1"
    process <- "milling"

    if (any(object[, , list(cereals, c(flour, "brans1", "branoil1"))] != 0)) {
      stop("Output flows already exist.")
    }

    milledGlobal   <- dimSums(object[, , list(cereals, milled)], dim = c("region", "ElementShort"))
    bransGlobal    <- dimSums(object[, , list(brans, "production")], dim = c("region", "ElementShort", "ItemCodeItem"))

    # as share of dm instaed of wm
    branAttributes <- bransGlobal / dimSums(bransGlobal[, , "dm"], dim = "attributes")

    # estimating bran based on simple factors (Feedipedia)
    # rice: 10%, wheat: 25%
    # we use 20% for wheat to account for some wholegrain meal
    # own estimates to not violate massbalance: corn and trce get only 5%
    branRatio <- new.magpie("GLO", getYears(object), cereals, fill = 0.20)
    getSets(branRatio) <- c("region", "year", "ItemCodeItem")
    branRatio[, , c("2804|Rice (Paddy Equivalent)", "2514|Maize and products")] <- 0.1
    branRatio[, , c("2518|Sorghum and products", "2517|Millet and products")]   <- 0.05
    bransUncalibrated <- dimSums(branRatio * milledGlobal[, , "dm"], dim = "ItemCodeItem")
    branRatio <- branRatio * dimSums(bransGlobal[, , "dm"], dim = "attributes") / bransUncalibrated

    # bran estimation
    branEstimated <- branRatio * branAttributes * dimSums(object[, , list(cereals, milled)][, , "dm"],
                                                          dim = "attributes")
    object[, , list(cereals, "brans1")]             <- dimSums(branEstimated[, , cereals], dim = c("ElementShort"))
    object[, , list(brans, "production_estimated")] <- dimSums(branEstimated[, , cereals],
                                                               dim = c("ItemCodeItem", "ElementShort"))
    object[, , list(cereals, flour)]                <- object[, , list(cereals, milled)] - branEstimated

    # branoil estimation
    .branoil1Production <- function(object, branoilItem, cropItem) {
      branoilRatio <- (dimSums(object[, , list(branoilItem, "production")],
                               dim = c("region", "ItemCodeItem", "ElementShort")) / dimSums(milledGlobal[, , cropItem],
                                                                                            dim = "ItemCodeItem"))
      estimatedBranoil <- object[, , list(cropItem, milled)] * branoilRatio
      object[, , list(cropItem, "branoil1")]                <- dimSums(estimatedBranoil[, , cropItem],
                                                                       dim = c("ElementShort"))
      object[, , list(branoilItem, "production_estimated")] <- dimSums(estimatedBranoil[, , cropItem],
                                                                       dim = c("ItemCodeItem", "ElementShort"))
      object[, , list(cropItem, flour)]                     <- object[, , list(cropItem, flour)] - estimatedBranoil
      return(object)
    }

    object <- .branoil1Production(object, "2582|Maize Germ Oil", "2514|Maize and products")
    object <- .branoil1Production(object, "2581|Ricebran Oil", "2804|Rice (Paddy Equivalent)")

    # check results and clear processed position
    object <- .checkAndClear(object, goodsIn = cereals, from = milled, process = process,
                             reportAs = c("brans1", "branoil1"), residual = flour)

    ### Fooduse in brans is included in the commodity balance sheets, but not reflected in calories.
    # We subtract bran consumption from cereal consumption in the respective countries.
    # For simplicity, we distribute brans proportional to all cereal fooduse.
    relAttributes <- c("wm", "ge", "nr")
    branshr <- (dimSums(object[, , list(brans, milled, relAttributes)], dim = c(3.1, 3.2))
                / dimSums(object[, , list(cereals, "households", relAttributes)], dim = c(3.1, 3.2)))
    branshr[is.nan(branshr)] <- 0
    if (any(branshr < 0)) {
      vcat(1, "branshr should not be smaller than zero.")
    }
    object[, , list(cereals, "households", relAttributes)] <- (1 - branshr) *
      object[, , list(cereals, "households", relAttributes)]
    object[, , list(brans, "households", relAttributes)]   <- object[, , list(brans, milled, relAttributes)]

    return(object)
  }

  # processing of tece and maiz (other_util) to ethanol, distillers grain and distilling loss
  .ethanolProcessing <- function(object) {
    "
    ethanol:
    DDGS Handbook
    U.S. Grains Council. 2013. A Guide to Distillers Dried Grains with Solubles (DDGS).
    http://www.grains.org/buyingselling/ddgs/handbook/20140422/comparison-different-grain-ddgs-sources-nutrient-composition. # nolint
    sugarcane: 654 l/t
    barley: 399 l/t
    corn: 408 l/t
    oats: 262 l/t
    wheat: 375 l/t
    ethanol weight per l:  789g
    similar numbers:
    Balat M and Balat H 2009 Recent trends in global production and utilization of bio-ethanol fuel Applied Energy
    86 2273-82
    "

    # Wheat instead of tece would be more correct, but we need to have homogeneous products
    tece <- .getFAOitems("tece")
    teceMaize <- c(tece, "2514|Maize and products")

    # liter yield for different sources
    ethanolYieldLiterPerTonTece <- 375
    ethanolYieldLiterPerTonMaize <- 408
    ethanolYieldLiterPerTonSugarcane <- 654

    # liter yield converted to dm (-> extraction factor)
    ethanolYieldLiterPerTonTeceMaize <- c(rep(ethanolYieldLiterPerTonTece, length(tece)), ethanolYieldLiterPerTonMaize)
    extractionQuantityTeceMaize <- 0.789 * ethanolYieldLiterPerTonTeceMaize / 1000
    extractionQuantitySugarcane <- 0.789 * ethanolYieldLiterPerTonSugarcane / 1000

    # ethanol processing from tece and maize (ethanol1, distillers_grain, and distillingloss)
    for (j in seq_along(teceMaize)) {
      object[, , c(teceMaize[j], "X001|Ethanol")] <- .extractGoodFromFlow(
        object = object[, , c(teceMaize[j], "X001|Ethanol")], # nolint
        goodIn = teceMaize[j],
        from = "other_util",
        process = "distilling",
        goodOut = "X001|Ethanol",
        reportAs = "ethanol1",
        residual = "intermediate",
        extractionQuantity = extractionQuantityTeceMaize[j],
        extractionAttribute = "dm",
        prodAttributes = prodAttributes)

      object[, , c(teceMaize[j], "X002|Distillers_grain")] <- .extractGoodFromFlow(
        object = object[, , c(teceMaize[j], "X002|Distillers_grain")], # nolint
        goodIn = teceMaize[j],
        from = "intermediate",
        process = "intermediate",
        goodOut = "X002|Distillers_grain",
        reportAs = "distillers_grain1",
        residual = "distillingloss",
        extractionQuantity = "max",
        extractionAttribute = "nr",
        prodAttributes = prodAttributes)
    }

    # ethanol processing from sugarcane (only ethanol1 and distillingloss)
    object[, , c("2536|Sugar cane", "X001|Ethanol")] <- .extractGoodFromFlow(
      object = object[, , c("2536|Sugar cane", "X001|Ethanol")], # nolint
      goodIn = "2536|Sugar cane",
      from = "other_util",
      process = "distilling",
      goodOut = "X001|Ethanol",
      reportAs = "ethanol1",
      residual = "distillingloss",
      extractionQuantity = extractionQuantitySugarcane,
      extractionAttribute = "dm",
      prodAttributes = prodAttributes)

    return(object)
  }

  # processing of tece (processed) to alcohol1 and alcoholloss
  .beerProcessing <- function(object) {
    # Barley would be more correct, but we need to have homogenous products
    beercereals <- .getFAOitems("tece")

    object[, , c(beercereals, "2656|Beer")] <- .processingGlobal(object = object[, , c(beercereals, "2656|Beer")],
                                                                 goodsIn = beercereals,
                                                                 from = "processed",
                                                                 process = "fermentation",
                                                                 goodsOut = "2656|Beer",
                                                                 reportAs = "alcohol1",
                                                                 residual = "intermediate")

    for (x in beercereals) {
      object[, , c(x, "X004|Brewers_grain")] <- .extractGoodFromFlow(object = object[, , c(x, "X004|Brewers_grain")],
                                                                     goodIn = x,
                                                                     from = "intermediate",
                                                                     process = "intermediate",
                                                                     goodOut = "X004|Brewers_grain",
                                                                     reportAs = "brewers_grain1",
                                                                     residual = "alcoholloss",
                                                                     extractionQuantity = "max",
                                                                     extractionAttribute = "dm",
                                                                     prodAttributes = prodAttributes)
    }

    return(object)
  }

  # processing of sugar cane and sugar beet (processed) to sugar1, molasses1 and refiningloss
  .sugarProcessing <- function(object) {

    goodsIn <- c("2536|Sugar cane", "2537|Sugar beet")
    goodsOut <- c("2818|Sugar, Refined Equiv", "2544|Molasses")
    object[, , c(goodsIn, goodsOut)] <- .processingGlobal(object = object[, , c(goodsIn, goodsOut)],
                                                          goodsIn = goodsIn,
                                                          from = "processed",
                                                          process = "refining",
                                                          goodsOut = goodsOut,
                                                          reportAs = c("sugar1", "molasses1"),
                                                          residual = "refiningloss")

    goodsIn <- c("2514|Maize and products")
    goodsOut <- c("2543|Sweeteners, Other")
    object[, , c(goodsIn, goodsOut)] <- .processingGlobal(object = object[, , c(goodsIn, goodsOut)],
                                                          goodsIn = goodsIn,
                                                          from = "processed",
                                                          process = "refining",
                                                          goodsOut = goodsOut,
                                                          reportAs = c("sugar1"),
                                                          residual = "refiningloss")

    return(object)
  }

  # processing of oil and oilcake from palm/palmkernel (processed)
  .oilpalmProcessing <- function(object) {
    # aggregate FAO products relating to oilpalm to a single raw product
    faoProductsOilpalm <- c("2577|Palm Oil", "2576|Palmkernel Oil", "2595|Palmkernel Cake")
    newproduct <- dimSums(object[, , list("production", faoProductsOilpalm, "dm")],
                          dim = c("ItemCodeItem", "ElementShort", "attributes"))
    newproduct <- prodAttributes[, , "X003|Palmoil_Kerneloil_Kernelcake"] * newproduct
    object[, , list("X003|Palmoil_Kerneloil_Kernelcake", c("production", "domestic_supply", "processed"))] <- newproduct

    # extract oil
    goodIn <- "X003|Palmoil_Kerneloil_Kernelcake"
    goodsOut1 <- c("2577|Palm Oil", "2576|Palmkernel Oil")
    goodsOut2 <- "2595|Palmkernel Cake"

    object[, , c(goodIn, goodsOut1)] <- .processingGlobal(object = object[, , c(goodIn, goodsOut1)],
                                                          goodsIn = goodIn,
                                                          from = "processed",
                                                          process = "extracting",
                                                          goodsOut = goodsOut1,
                                                          reportAs = c("oil1", "oil2"),
                                                          residual = "intermediate")

    object[, , c(goodIn, goodsOut2)] <- .extractGoodFromFlow(object = object[, , c(goodIn, goodsOut2)],
                                                             goodIn = goodIn,
                                                             from = "intermediate",
                                                             process = "intermediate",
                                                             goodOut = goodsOut2,
                                                             reportAs = "oilcakes1",
                                                             residual = "extractionloss",
                                                             extractionQuantity = "max",
                                                             extractionAttribute = "dm",
                                                             prodAttributes = prodAttributes)

    return(object)
  }

  # extraction of oil and oilcakes from oilcrops (processed)
  .oilProcessing <- function(object) {
    # orders must match!
    cropsIn <- c("2555|Soyabeans", "2820|Groundnuts (in Shell Eq)", "2557|Sunflower seed",
                 "2559|Cottonseed", "2558|Rape and Mustardseed", "2560|Coconuts - Incl Copra",
                 "2561|Sesame seed")
    oilOut <- c("2571|Soyabean Oil", "2572|Groundnut Oil", "2573|Sunflowerseed Oil",
                "2575|Cottonseed Oil", "2574|Rape and Mustard Oil", "2578|Coconut Oil",
                "2579|Sesameseed Oil")
    cakeOut <- c("2590|Soyabean Cake", "2591|Groundnut Cake", "2592|Sunflowerseed Cake",
                 "2594|Cottonseed Cake", "2593|Rape and Mustard Cake", "2596|Copra Cake",
                 "2597|Sesameseed Cake")

    otherCropsIn <- c("2570|Oilcrops, Other", "2563|Olives (including preserved)")
    otherOilOut <- "2586|Oilcrops Oil, Other"
    otherCakeOut <- "2598|Oilseed Cakes, Other"

    # main oil crops
    for (j in seq_along(cropsIn)) {
      object[, , c(cropsIn[j], oilOut[j])] <- .processingGlobal(object = object[, , c(cropsIn[j], oilOut[j])],
                                                                goodsIn = cropsIn[j],
                                                                from = "processed",
                                                                process = "extracting",
                                                                goodsOut = oilOut[j],
                                                                reportAs = "oil1",
                                                                residual = "intermediate")
      object[, , c(cropsIn[j], cakeOut[j])] <- .extractGoodFromFlow(object = object[, , c(cropsIn[j], cakeOut[j])],
                                                                    goodIn = cropsIn[j],
                                                                    from = "intermediate",
                                                                    process = "intermediate",
                                                                    goodOut = cakeOut[j],
                                                                    reportAs = "oilcakes1",
                                                                    residual = "extractionloss",
                                                                    extractionQuantity = "max",
                                                                    extractionAttribute = "dm",
                                                                    prodAttributes = prodAttributes)
    }


    # other oil crops
    object[, , c(otherCropsIn, otherOilOut)] <- .processingGlobal(object = object[, , c(otherCropsIn, otherOilOut)],
                                                                  goodsIn = otherCropsIn,
                                                                  from = "processed",
                                                                  process = "extracting",
                                                                  goodsOut = otherOilOut,
                                                                  reportAs = "oil1",
                                                                  residual = "intermediate")

    for (goodIn in otherCropsIn) {
      object[, , c(goodIn, otherCakeOut)] <- .extractGoodFromFlow(object = object[, , c(goodIn, otherCakeOut)],
                                                                  goodIn = goodIn,
                                                                  from = "intermediate",
                                                                  process = "intermediate",
                                                                  goodOut = otherCakeOut,
                                                                  reportAs = "oilcakes1",
                                                                  residual = "extractionloss",
                                                                  extractionQuantity = "max",
                                                                  extractionAttribute = "dm",
                                                                  prodAttributes = prodAttributes)
    }

    return(object)
  }

  # main function combining all processing functions
  .massbalanceProcessing <- function(years) {
    # preparing dataset for given years
    cells <- getCells(cbc)
    s1 <- getNames(cbc, dim = 1)
    s2 <- c(getNames(cbc, dim = 2), namesProcessing)
    s3 <- attributeTypes
    flowsCBC <- array(dim = c(length(cells), length(years), length(s1), length(s2), length(s3)),
                      dimnames = list(cells, years, s1, s2, s3))
    flowsCBC <- as.magpie(flowsCBC)
    getSets(flowsCBC) <- c("region", "year", "ItemCodeItem", "ElementShort", "attributes")
    flowsCBC[, , getNames(cbc, dim = 2)] <- cbc[, years, ] * attributesWM[, , getNames(cbc, dim = 1)]
    gc()

    # conversion from 10^12 kcal to PJ
    flowsCBC[, , list("households", "ge")] <- cbc[, years, "food_supply_kcal"] * 4.184
    # conversion of protein to nitrogen using average N content
    flowsCBC[, , list("households", "nr")] <- cbc[, years, "protein_supply"] / 6.25
    flowsCBC[, , list("households", "wm")] <- cbc[, years, "food_supply"]

    flowsCBC <- flowsCBC[, , setdiff(getNames(flowsCBC, dim = "ElementShort"),
                                     c("food_supply_kcal", "protein_supply", "food_supply", "fat_supply"))]
    flowsCBC[is.na(flowsCBC)]  <- 0
    flowsCBC[is.nan(flowsCBC)] <- 0
    gc()

    # relevant processing dimensions
    millingDimensions <- c("production", "production_estimated", "milling", "brans1",
                           "branoil1", "flour1", "food", "households")
    distillingDimensions <- c("production", "production_estimated", "other_util", "distilling",
                              "ethanol1", "intermediate", "distillers_grain1", "distillingloss")
    fermentationDimensions <- c("production", "production_estimated", "processed", "fermentation",
                                "alcohol1", "intermediate", "brewers_grain1", "alcoholloss")
    refiningDimensions <- c("production", "production_estimated", "processed", "sugar1",
                            "molasses1", "refining", "refiningloss")
    extractingDimensions <- c("production", "production_estimated", "domestic_supply", "processed",
                              "extracting", "oil1", "oil2", "intermediate", "oilcakes1", "extractionloss")

    # relevant processing products
    millingProducts <- c(.getFAOitems(c("tece", "maiz", "rice_pro", "trce", "brans")),
                         "2582|Maize Germ Oil", "2581|Ricebran Oil")
    distillingProducts <- c(.getFAOitems(c("tece", "maiz", "sugr_cane", "ethanol")), "X002|Distillers_grain")
    fermentationProducts <- c(.getFAOitems("tece"), "2656|Beer", "X004|Brewers_grain")
    refiningProducts <- c(.getFAOitems(c("sugr_cane", "sugr_beet", "maiz", "molasses")),
                          "2818|Sugar, Refined Equiv", "2543|Sweeteners, Other")
    extractingProducts1 <- c("2577|Palm Oil", "2576|Palmkernel Oil", "2595|Palmkernel Cake",
                             "X003|Palmoil_Kerneloil_Kernelcake")
    extractingProducts2 <- setdiff(.getFAOitems(c("soybean", "groundnut", "sunflower", "cottn_pro",
                                                  "rapeseed", "oils", "oilcakes")),
                                   c(extractingProducts1, "2580|Olive Oil", "2581|Ricebran Oil", "2582|Maize Germ Oil"))


    # Food processing calculations
    flowsCBC[, , list(millingProducts, millingDimensions)] <-
      .cerealMillingGlobal(flowsCBC[, , list(millingProducts, millingDimensions)])
    flowsCBC[, , list(distillingProducts, distillingDimensions)] <-
      .ethanolProcessing(flowsCBC[, , list(distillingProducts, distillingDimensions)])
    flowsCBC[, , list(fermentationProducts, fermentationDimensions)] <-
      .beerProcessing(flowsCBC[, , list(fermentationProducts, fermentationDimensions)])
    flowsCBC[, , list(refiningProducts, refiningDimensions)] <-
      .sugarProcessing(flowsCBC[, , list(refiningProducts, refiningDimensions)])
    flowsCBC[, , list(extractingProducts1, extractingDimensions)] <-
      .oilpalmProcessing(flowsCBC[, , list(extractingProducts1, extractingDimensions)])
    flowsCBC[, , list(extractingProducts2, extractingDimensions)] <-
      .oilProcessing(flowsCBC[, , list(extractingProducts2, extractingDimensions)])

    # harmonizing conversion factors within the rapeseed group
    goodsIn  <- list("2558|Rape and Mustardseed", "2560|Coconuts - Incl Copra", "2561|Sesame seed",
                     c("2570|Oilcrops, Other", "2563|Olives (including preserved)"))
    oilsOut <- list("2574|Rape and Mustard Oil", "2578|Coconut Oil", "2579|Sesameseed Oil", "2586|Oilcrops Oil, Other")
    cakesOut <- list("2593|Rape and Mustard Cake", "2596|Copra Cake", "2597|Sesameseed Cake",
                     "2598|Oilseed Cakes, Other")

    for (from in c("oil1", "oilcakes1", "extractionloss")) {
      factor <- dimSums(flowsCBC[, , list(unlist(goodsIn), from)], dim = c(1, 3.1, 3.2)) /
        dimSums(flowsCBC[, , list(unlist(goodsIn), "extracting")], dim = c(1, 3.1, 3.2))
      flowsCBC[, , list(unlist(goodsIn), from)] <- factor * dimSums(flowsCBC[, , list(unlist(goodsIn), "extracting")],
                                                                    dim = 3.2)
      gc()
    }

    for (j in seq_along(oilsOut)) {
      flowsCBC[, , list(oilsOut[[j]], "production_estimated")] <- dimSums(flowsCBC[, , list(goodsIn[[j]], "oil1")],
                                                                          dim = c(3.1, 3.2))
      flowsCBC[, , list(cakesOut[[j]], "production_estimated")] <- dimSums(flowsCBC[, , list(goodsIn[[j]],
                                                                                             "oilcakes1")],
                                                                           dim = c(3.1, 3.2))
      gc()
    }

    # Alcohol production
    cropsAlcohol <- .getFAOitems(c("others", "trce", "rice_pro", "potato", "cassav_sp", "sugar", "molasses", "brans"))
    fermentationDimensions <- c("production", "production_estimated", "processed", "fermentation", "alcohol1",
                                "alcohol2", "alcohol3", "alcohol4", "intermediate", "brewers_grain1", "alcoholloss")
    fermentationProducts <- .getFAOitems(c("tece", "others", "trce", "rice_pro", "potato", "cassav_sp", "sugar",
                                           "molasses", "brans", "alcohol", "distillers_grain"))
    flowsCBC[, , list(fermentationProducts, fermentationDimensions)] <- .processingGlobal(
      flowsCBC[, , list(fermentationProducts, fermentationDimensions)], # nolint
      goodsIn  = cropsAlcohol,
      from      = "processed",
      process   = "fermentation",
      goodsOut = c("2655|Wine", "2657|Beverages, Fermented",
                   "2658|Beverages, Alcoholic", "2659|Alcohol, Non-Food"),
      reportAs = c("alcohol1", "alcohol2", "alcohol3", "alcohol4"), # nolint
      residual  = "alcoholloss")

    # Define use of products that are not existing in FAOSTAT
    goods <- c("X002|Distillers_grain", "X004|Brewers_grain")
    flowsCBC[, , list(goods, c("production", "domestic_supply", "feed"))]  <-
      flowsCBC[, , list(goods, "production_estimated"), drop = TRUE]
    flowsCBC[, , list("X001|Ethanol", c("production", "domestic_supply", "other_util"))] <-
      flowsCBC[, , list("X001|Ethanol", "production_estimated"), drop = TRUE]
    gc()

    # add remaining 'processed' to 'other_util' and remove obsolete dimensions
    flowsCBC[, , "other_util"] <- dimSums(flowsCBC[, , c("other_util", "processed")], dim = 3.2)
    flowsCBC <- flowsCBC[, , c("processed", "intermediate"), invert = TRUE]
    gc()

    # map to magpie categories
    massbalanceProcessing <- toolAggregate(x = flowsCBC,
                                           rel = relationmatrix,
                                           dim = 3.1,
                                           from = "FoodBalanceItem",
                                           to = "k",
                                           partrel = TRUE)
    gc()
    return(massbalanceProcessing)
  }

  # function dealing with the non-processing aspects of cbc
  .massbalanceNoProcessing <- function(years) {
    # initializing magpie object
    cells <- getCells(cbc)
    s2 <- c(getNames(cbc, dim = 2), namesProcessing)
    s3 <- attributeTypes
    noProcessingCBC <- array(dim = c(length(cells), length(years), length(noProcessingFAO), length(s2),
                                     length(s3)), dimnames = list(cells, years, noProcessingFAO, s2, s3))
    noProcessingCBC <- as.magpie(noProcessingCBC)
    getSets(noProcessingCBC) <- c("region", "year", "ItemCodeItem", "ElementShort", "attributes")

    # adding attributes and filling household dimension
    noProcessingCBC[, , getNames(cbc, dim = 2)] <- cbc[, years, noProcessingFAO] * attributesWM[, , noProcessingFAO]
    # conversion from 10^12 kcal to PJ
    noProcessingCBC[, , list("households", "ge")] <- noProcessingCBC[, , list("food_supply_kcal", "wm")] * 4.184
    # conversion of protein to nitrogen using average N content
    noProcessingCBC[, , list("households", "nr")] <- noProcessingCBC[, , list("protein_supply", "wm")] / 6.25
    noProcessingCBC[, , list("households", "wm")] <- noProcessingCBC[, , list("food_supply", "wm")]
    noProcessingCBC <- noProcessingCBC[, , setdiff(getNames(noProcessingCBC, dim = "ElementShort"),
                                                   c("food_supply_kcal", "protein_supply",
                                                     "food_supply", "fat_supply"))]

    # fill NAs and NaNs
    noProcessingCBC[is.na(noProcessingCBC)]  <- 0
    noProcessingCBC[is.nan(noProcessingCBC)] <- 0

    # add 'processed' to 'other_util' and remove obsolete dimensions
    noProcessingCBC[, , "other_util"] <- dimSums(noProcessingCBC[, , c("other_util", "processed")], dim = 3.2)
    noProcessingCBC <- noProcessingCBC[, , c("processed", "intermediate"), invert = TRUE]

    # map to magpie categories
    massbalanceNoProcessing <- toolAggregate(x = noProcessingCBC,
                                             rel = relationmatrix,
                                             dim = 3.1,
                                             from = "FoodBalanceItem",
                                             to = "k",
                                             partrel = TRUE)
    gc()
    return(massbalanceNoProcessing)
  }


  #### Calculations ####

  # increase magclass sizelimit
  local_options(magclass_sizeLimit = 2e8)

  # option without splitting years (in case of memory issues this can be done in year chunks)
  massbalanceNoProcessing <- .massbalanceNoProcessing(years)
  cbc <- cbc[, , noProcessingFAO, invert = TRUE]
  massbalanceProcessing <- .massbalanceProcessing(years)

  # put results together
  massbalance <- mbind(massbalanceProcessing, massbalanceNoProcessing)

  return(list(x = massbalance,
              weight = NULL,
              unit = "MT C, Mt DM, PJ, Mt K, Mt Nr, Mt P, Mt WM",
              description = paste("FAO massbalance calculates all conversion processes within the FAO CBS/FBS and",
                                  "makes them explict. More complete version can be found in calcFAOmassbalance")))
}
