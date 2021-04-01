#' @title calcFAOmassbalance_pre
#' @description Calculates an extended version of the Food Balance Sheets. Makes explicit the conversion processes that convert one type of product into another. 
#' Includes processes like milling, distilling, extraction etc. Adds certain byproducts like distillers grains or ethanol.
#'
#' @param years years to be estimated, if null, then all years in FAOharmonized are returned
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' This is an intermediary result, which is used e.g. for estimating the feed baskets. For most uses, it is more appropriate to use the FAOmasbalance instead of the FAOmassbalance_pre.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcFAOmassbalance}}
#' @examples
#'
#' \dontrun{
#' calcOutput("FAOmassbalance_pre")
#' }
#' @importFrom graphics plot
#' @importFrom magclass getSets as.magpie fulldim complete_magpie
#' @importFrom utils read.csv
#' @importFrom madrat madlapply

calcFAOmassbalance_pre <- function(years = NULL) {
  #### Data input: FAO Commodity Balance ####
  CBC          <- calcOutput(type = "FAOharmonized", aggregate = FALSE)
  getSets(CBC) <- c("region", "year", "ItemCodeItem.ElementShort")
  
  if (any(duplicated(dimnames(CBC)[[3]]) == TRUE)) {
    stop("The folowing dimnames are duplicated: ", paste(getNames(CBC)[duplicated(getNames(CBC))], collapse = "\", \""))  
  }
  
  # remove double counting  
  removethem<-c(
    #crop commodity balance and Food Supply items aggregated
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
    #livestock commodity balance and Food Supply items aggregated
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
    #others and equivalents
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
  
  CBC <- CBC[, , removethem, invert = TRUE]
  
  CBC <- complete_magpie(CBC, fill = 0)
  
  missingproducts <- c("X001|Ethanol", 
                       "X002|Distillers_grain", 
                       "X003|Palmoil_Kerneloil_Kernelcake", 
                       "X004|Brewers_grain")
  
  CBC <- add_columns(CBC, addnm = missingproducts, dim = 3.1)
  
  #### Data input: Production Attributes ####
  prod_attributes <- calcOutput("Attributes", aggregate = FALSE)
  attribute_types <- getNames(prod_attributes, dim  = 1)
  remove_prod     <-  c("betr", "begr", "pasture", "scp", "res_cereals", 
                        "res_fibrous", "res_nonfibrous", "wood", "woodfuel")
  prod_attributes <- prod_attributes[, , remove_prod, invert = TRUE]
  
  # Sectoral mapping for FAO items 
  relationmatrix <- toolGetMapping("FAOitems_online.csv", type = "sectoral", where = "mappingfolder")
  relationmatrix <- relationmatrix[, c("FoodBalanceItem", "k")]
  relationmatrix <- relationmatrix[!duplicated(relationmatrix[, "FoodBalanceItem"]), ]
  
  # Map production attributes to FAO items
  prod_attributes          <- toolAggregate(x = prod_attributes, rel = relationmatrix, dim = 3.2, from = "k", to = "FoodBalanceItem", partrel = TRUE)
  getSets(prod_attributes) <- c("region", "year", "attributes", "ItemCodeItem")
  attributes_wm            <- (prod_attributes / dimSums(prod_attributes[, , "wm"], dim = "attributes")) # change prod attributes from share of dm to share of wm
  
  itemnames_CBC        <- getNames(CBC, dim = "ItemCodeItem")
  itemnames_attributes <- getNames(attributes_wm, dim = "ItemCodeItem")
  if (!(all(itemnames_CBC %in% itemnames_attributes))) {
    vcat(verbosity = 2, "The following items were removed from the dataset because of missing prod_attributes: ", 
         paste(itemnames_CBC[!(itemnames_CBC %in% itemnames_attributes)], collapse = "\", \""))
    CBC <- CBC[, , itemnames_CBC[itemnames_CBC %in% itemnames_attributes]]
  }
  if (!all(itemnames_attributes %in% itemnames_CBC)) {
    stop("For the following items there were entries in prod_attributes but no respective data: ", 
         paste(itemnames_attributes[!(itemnames_attributes %in% itemnames_CBC)], collapse = "\", \""))
  }
  
  #### Definition of subfunctions #####
  
  # run massbalance checks and clear processed positions after calculating process
  .check_and_clear <- function(object, 
                               goods_in,
                               from,
                               process,
                               report_as,
                               residual,
                               relevant_attributes = attribute_types, 
                               goods_out = NULL, 
                               threshold = 1e-5) {
    
    # perform massbalance tests:
    # 1) input goods balanced?
    diff <- (dimSums(object[, , list(goods_in, c(report_as, residual))], dim = c("ElementShort"))
             - dimSums(object[, , list(goods_in, from)], dim = c("ElementShort")))
    if (any(abs(diff) > threshold)) {
      stop("NAs in dataset or function corrupt: process not balanced for ", 
           paste(goods_in, collapse = ", "), " reported as ", paste(report_as, collapse = ", "))
    }
    
    # 2) output goods balanced...
    if (!is.null(goods_out)) {
      # ... with input goods?
      diff <- (dimSums(object[, , list(goods_in, from)], dim = c("ElementShort", "ItemCodeItem"))
               - dimSums(object[, , list(goods_in, residual)], dim = c("ElementShort", "ItemCodeItem"))
               - dimSums(object[, , list(goods_out, "production_estimated")], dim = c("ElementShort", "ItemCodeItem"))
      )
      if (any(abs(diff) > threshold)) {
        stop("NAs in dataset or function corrupt: goods not balanced for ", 
             paste(goods_out, collapse = ", "), " from ", paste(goods_in, collapse = ", "))
      }
      
      # ... in production?
      diff <- (sum(object[, , list(goods_out, "production_estimated")]) 
               - sum(object[, , list(goods_out, "production")]))
      if (any(abs(diff) > threshold)) {
        stop("Global estimated production does not meet global production for ",
             paste(goods_out, collapse = ", "))
      }
    }
    
    # 4) special case for cereal milling: branoils balanced?
    if (residual == "flour1") {
      diff <- (dimSums(object[, , list(goods_in, "branoil1")], dim = c("ElementShort", "ItemCodeItem"))
               - dimSums(object[, , list(c("2581|Ricebran Oil", "2582|Maize Germ Oil"), "production_estimated")], dim = c("ElementShort", "ItemCodeItem")))
      if (any(abs(diff) > threshold)) {
        stop("NAs in dataset or function corrupt: branoil1 not balanced")
      }
    }
    
    # negative value check
    if (any(object[, , list(goods_in, c(report_as, from, residual), relevant_attributes)] < -threshold)) {
      warning("Massbalancing failed, negative values for ", 
              paste(unique(unname(where(object[, , list(goods_in, c(report_as, from, residual), relevant_attributes)] < -threshold)[[1]]$individual[,3])), collapse = ", "))
    }
    
    # move from "from" to "process" and clear "from"
    if (from != process) {
      object[, , list(goods_in, process)] <- object[, , list(goods_in, from)]
    }
    object[, , list(goods_in, from)] <- 0  # if from == process it is "intermediate" which is to be cleared as well
    
    return(object)
  }
  
  # Different processes, e.g. ethanol production from cereals, are not specified 
  # in CBC (instead the general categories "other_util" and "processed" are used),
  # but are required within MAgPIE. 
  # This function calculates the maximum amount out of a given product "good_in.from" 
  # that can be used to produce a given output product "good_out", depending on 
  # the "extraction_quantity" and the "extraction_attribute". This quantity is 
  # reported as "good_in.report_as", while any remaining quantity in "good_in.from" 
  # is reported as "good_in.residual". The full amount in "good_in.from" is then 
  # moved to "good_in.process" (i.e. "good_in.from" will be empty after the function 
  # call). The variable "process" specifies the process leading to the output product 
  # "report_as". 
  # The calculated production quantity is also added to "good_out.production_estimated". 
  .extract_good_from_flow <- function(object,
                                      good_in,              # FAO-defined input product, e.g. "2536|Sugar cane"
                                      from,                 # FAO-defined process, e.g. "other_util"
                                      process,              # MAgPIE-defined process, e.g. "distilling"
                                      good_out,             # FAO-defined output product, e.g. "X001|Ethanol"
                                      report_as,            # MAgPIE-defined output product, e.g. "ethanol1"
                                      residual,             # MAgPIE-defined residual, e.g. "distillingloss"
                                      extraction_quantity,  # e.g. 0.516006
                                      extraction_attribute, # e.g. "dm"
                                      prod_attributes) {
    
    if (length(from) > 1 || length(report_as) > 1 || length(good_in) > 1 || length(good_out) > 1) {
      stop("please only use one item each for \"from\", \"report_as\", \"good_in\", and \"good_out\"" )
    }
    if (any(object[, , list(good_in, c(report_as, residual))] != 0)) {
      warning("Output flows already exist!")
    }
    
    # relevant attributes for extraction quantity
    attr_no_wm <- setdiff(attribute_types, "wm")
    
    # calculating possible extraction quantity per attribute
    attributes_from   <- dimSums(object[, , list(good_in, from), drop = T], dim = "region") / dimSums(object[, , list(good_in, from, extraction_attribute), drop = TRUE], dim = c("region"))
    attributes_to     <- prod_attributes[, , good_out, drop = T] / prod_attributes[, , list(good_out, extraction_attribute), drop = TRUE]
    extraction_factor <- attributes_from[, , attr_no_wm] / attributes_to[, , attr_no_wm]
    
    # maaximum extraction quantity as minimum over the possible quantity per attribute
    maxextract <- as.magpie(apply(X = extraction_factor, MARGIN = 2, FUN = min))
    if (extraction_quantity == "max") {
      extraction_quantity <- maxextract
    } else if (any(extraction_quantity > maxextract)) {
      stop("too high extraction quantity")
    }
    
    # calculate outputs
    extracted <- object[, , list(good_in, from, extraction_attribute), drop = TRUE] * extraction_quantity * attributes_to
    losses    <- dimSums(object[, , list(good_in, from)], dim = "ElementShort") - extracted
    
    object[, , list(good_in, report_as)] <- extracted
    object[, , list(good_in, residual)]  <- losses
    
    object[, , list(good_out, "production_estimated")] <- object[, , list(good_out, "production_estimated")] + extracted
    
    # check results and clear processed position
    object <- .check_and_clear(object, good_in, from, process, report_as, residual, attr_no_wm) 
    
    return(object)
  }
  
  # This function is similar to .extract_good_from_flow, with the difference that
  # multiple input goods can be given (which will then be added up before calculating
  # the amount of "goods_out" that can be produced), and that multiple output goods 
  # (and corresponding items in report_as) can be given. The order of FAO categories
  # in "goods_out" and corresponding MAgPIE categories in "report_as" needs to match!
  # In contrast to .extract_good_from_flow, this function calculates global 
  # conversion factors per attribute instead of using an "extraction_quantity" 
  # and "extraction_attribute" for calculations.
  .processing_global <- function(object,
                                 goods_in,      # e.g. c("2536|Sugar cane", "2537|Sugar beet")
                                 from,          # e.g. "processed"
                                 process,       # e.g. "refining"
                                 goods_out,     # e.g. c("2818|Sugar, Refined Equiv", "2544|Molasses") - (the order matters!)
                                 report_as,     # e.g. c("sugar1", "molasses1") - (the order matters!)
                                 residual       # e.g. "refiningloss"
                                 ) {
    if (any(object[, , list(goods_in, c(report_as, residual))] != 0)) {
      stop("Output flows already exist.")
    }
    if (any(object[, , list(goods_out, "production_estimated")] != 0)) {
      stop("Output flows already exist.")
    }
    
    # attributes relevant for checking massbalance and conv_factor
    relevant_attributes <- setdiff(attribute_types, "wm")
    
    # calculate global conversion factor per attributes
    conv_factor <- (dimSums(object[, , list(goods_out, "production")], dim = c("region", "ElementShort"))
                    / dimSums(object[, , list(goods_in, from)], dim = c("region", "ItemCodeItem", "ElementShort")))
    
    if (any(dimSums(conv_factor[, , list(goods_out, relevant_attributes)], dim = "ItemCodeItem") > 1)) {
      stop("conversion factors exceed 1. not suitable for a global conversion factor.",
           paste(unique(unname(where(dimSums(conv_factor[, , list(goods_out, relevant_attributes)], dim = "ItemCodeItem") > 1)[[1]]$individual)), collapse = ", "))
    }
    
    # estimate outputs
    .estim_outputs <- function(j) {
      object[, , list(goods_in, report_as[j])] <<- dimSums(object[, , list(goods_in, from)], dim = "ElementShort") * conv_factor[, , goods_out[j], drop = TRUE]
      object[, , list(goods_out[j], "production_estimated")] <<- dimSums(object[, , list(goods_in, report_as[j])], dim = c("ElementShort", "ItemCodeItem"))
    }
    
    invisible(lapply(c(1:length(goods_out)), .estim_outputs))
    
    # calculate refining losses as mass balance difference
    object[, , list(goods_in, residual)] <- (dimSums(object[, , list(goods_in, from)], dim = c("ElementShort"))
                                             - dimSums(object[, , list(goods_in, report_as)], dim = c("ElementShort")))
    
    # check results and clear processed position
    object <- .check_and_clear(object, goods_in, from, process, report_as, residual, relevant_attributes, goods_out)
    
    return(object)
  }
  
  
  # processing of cereals (milled) to bran and flour: this is the only process that does 
  # not use the functions .extract_good_from_flow and/or .processing_global,
  # as the extraction quantity of bran is calculated in a specific way, using the
  # ratio of bran to full cereal as given by Feedipedia.
  .cereal_milling_global <- function(object) {
    
    cereals <- c("2511|Wheat and products", 
                 "2513|Barley and products", 
                 "2514|Maize and products",
                 "2515|Rye and products", 
                 "2516|Oats", 
                 "2517|Millet and products",
                 "2518|Sorghum and products", 
                 "2520|Cereals, Other", 
                 "2804|Rice (Paddy Equivalent)")
    
    brans   <- relationmatrix[relationmatrix[, 2] == "brans", 1]
    milled  <- "food"
    flour   <- "flour1"
    process <- "milling"
    
    if (any(object[, , list(cereals, c(flour, "brans1", "branoil1"))] != 0)) {
      stop("Output flows already exist.")
    }
    
    milled_global   <- dimSums(object[, , list(cereals, milled)], dim = c("region", "ElementShort"))
    brans_global    <- dimSums(object[, , list(brans, "production")], dim = c("region", "ElementShort", "ItemCodeItem"))
    bran_attributes <- brans_global / dimSums(brans_global[, , "dm"], dim = "attributes") # as share of dm instaed of wm
    
    # estimating bran based on simple factors (Feedipedia)
    # rice: 10%, wheat: 25%
    # we use 20% for wheat to account for some wholegrain meal
    # own estimates to not violate massbalance: corn and trce get only 5%
    bran_ratio <- new.magpie("GLO", years, cereals, fill = 0.20)
    getSets(bran_ratio) <- c("region", "year", "ItemCodeItem")
    bran_ratio[, , c("2804|Rice (Paddy Equivalent)", "2514|Maize and products")] <- 0.1
    bran_ratio[, , c("2518|Sorghum and products", "2517|Millet and products")]   <- 0.05
    brans_uncalibrated <- dimSums(bran_ratio * milled_global[, , "dm"], dim = "ItemCodeItem")
    bran_ratio <- bran_ratio * dimSums(brans_global[, , "dm"], dim = "attributes") / brans_uncalibrated
    
    # bran estimation
    bran_estimated <- bran_ratio * dimSums(object[, , list(cereals, milled)][, , "dm"], dim = "attributes") * bran_attributes
    object[, , list(cereals, "brans1")]             <- dimSums(bran_estimated[, , cereals], dim = c("ElementShort"))
    object[, , list(brans, "production_estimated")] <- dimSums(bran_estimated[, , cereals], dim = c("ItemCodeItem", "ElementShort"))
    object[, , list(cereals, flour)]                <- object[, , list(cereals, milled)] - bran_estimated
    
    # branoil estimation
    .branoil1_production <- function(object, branoilItem, cropItem) {
      branoil_ratio <- (dimSums(object[, , list(branoilItem, "production")], dim = c("region", "ItemCodeItem", "ElementShort"))  
                        / dimSums(milled_global[, , cropItem], dim = "ItemCodeItem"))
      estimated_branoil <- object[, , list(cropItem, milled)] * branoil_ratio
      object[, , list(cropItem, "branoil1")]                <- dimSums(estimated_branoil[, , cropItem], dim = c("ElementShort"))
      object[, , list(branoilItem, "production_estimated")] <- dimSums(estimated_branoil[, , cropItem], dim = c("ItemCodeItem", "ElementShort"))
      object[, , list(cropItem, flour)]                     <- object[, , list(cropItem, flour)] - estimated_branoil
      return(object)
    }
    
    object <- .branoil1_production(object, "2582|Maize Germ Oil", "2514|Maize and products")
    object <- .branoil1_production(object, "2581|Ricebran Oil", "2804|Rice (Paddy Equivalent)")
    
    # check results and clear processed position
    object <- .check_and_clear(object, cereals, milled, process, c("brans1", "branoil1"), flour)
    
    ### Fooduse in brans is included in the commodity balance sheets, but not reflected in calories.
    # We subtract bran consumption from cereal consumption in the respective countries.
    # For simplicity, we distribute brans proportional to all cereal fooduse.
    branshr <- (dimSums(object[, , list(brans, milled, c("wm", "ge", "nr"))], dim = c(3.1, 3.2)) 
                / dimSums(object[, , list(cereals, "households", c("wm", "ge", "nr"))], dim = c(3.1, 3.2)))
    branshr[is.nan(branshr)] <- 0
    if (any(branshr < 0)) {
      vcat(1, "branshr should not be smaller than zero.")
    }
    object[, , list(cereals, "households", c("wm", "ge", "nr"))] <- (1 - branshr) * object[, , list(cereals, "households", c("wm", "ge", "nr"))]
    object[, , list(brans, "households", c("wm", "ge", "nr"))]   <- object[, , list(brans, milled, c("wm", "ge", "nr"))]
    
    return(object)
  }
  
  # processing of tece and maiz (other_util) to ethanol, distillers grain and distilling loss
  .ethanol_processing <- function(object) {
    "  
    ethanol:
    DDGS Handbook
    U.S. Grains Council. 2013. A Guide to Distillers Dried Grains with Solubles (DDGS). 
    http://www.grains.org/buyingselling/ddgs/handbook/20140422/comparison-different-grain-ddgs-sources-nutrient-composition.
    sugarcane: 654 l/t
    barley: 399 l/t
    corn: 408 l/t
    oats: 262 l/t
    wheat: 375 l/t
    ethanol weight per l:  789g
    similar numbers: 
    Balat M and Balat H 2009 Recent trends in global production and utilization of bio-ethanol fuel Applied Energy 86 2273-82
    "
    
    # Wheat instead of tece would be more correct, but we need to have homogeneous products
    tece <- relationmatrix[relationmatrix[, 2] == "tece", 1]
    tece_maize <- c(tece, "2514|Maize and products")
    
    # liter yield for different sources
    ethanol_yield_liter_per_ton_tece      <- 375
    ethanol_yield_liter_per_ton_maize     <- 408
    ethanol_yield_liter_per_ton_sugarcane <- 654
    
    # liter yield converted to dm (-> extraction factor)
    ethanol_yield_liter_per_ton_tece_maize <- c(rep(ethanol_yield_liter_per_ton_tece, length(tece)), ethanol_yield_liter_per_ton_maize)
    extraction_quantity_tece_maize         <- 0.789 * ethanol_yield_liter_per_ton_tece_maize / 1000
    extraction_quantity_sugarcane          <- 0.789 * ethanol_yield_liter_per_ton_sugarcane / 1000
    
    # ethanol processing from tece and maize (ethanol1, distillers_grain, and distillingloss)
    .extract_grain_loss <- function(j) {
      object <<- .extract_good_from_flow(object = object,
                                         good_in = tece_maize[j],
                                         from = "other_util",
                                         process = "distilling",
                                         good_out = "X001|Ethanol",
                                         report_as = "ethanol1",
                                         residual = "intermediate",
                                         extraction_quantity = extraction_quantity_tece_maize[j],
                                         extraction_attribute = "dm",
                                         prod_attributes = prod_attributes)
      
      object <<- .extract_good_from_flow(object = object,
                                         good_in = tece_maize[j],
                                         from = "intermediate",
                                         process = "intermediate",
                                         good_out = "X002|Distillers_grain",
                                         report_as = "distillers_grain1",
                                         residual = "distillingloss",
                                         extraction_quantity = "max",
                                         extraction_attribute = "nr", 
                                         prod_attributes = prod_attributes) 
    }
    invisible(lapply(1:length(tece_maize), .extract_grain_loss))
    
    # ethanol processing from sugarcane (only ethanol1 and distillingloss)
    object <- .extract_good_from_flow(object = object,
                                      good_in = "2536|Sugar cane",
                                      from = "other_util",
                                      process = "distilling",
                                      good_out = "X001|Ethanol",
                                      report_as = "ethanol1",
                                      residual = "distillingloss",
                                      extraction_quantity = extraction_quantity_sugarcane,
                                      extraction_attribute = "dm",
                                      prod_attributes = prod_attributes)
    
    return(object)
  }
  
  # processing of tece (processed) to alcohol1 and alcoholloss
  .beer_processing <- function(object) {
    # Barley would be more correct, but we need to have homogenous products
    beercereals <- relationmatrix[relationmatrix[, 2] == "tece", 1]
    
    object <- .processing_global(object = object,
                                 goods_in = beercereals,
                                 from = "processed",
                                 process = "fermentation",
                                 goods_out = "2656|Beer",
                                 report_as = "alcohol1",
                                 residual = "intermediate")
    
    .add_beercereals <- function(x) {
      object <<- .extract_good_from_flow(object = object,
                                         good_in = x,
                                         from = "intermediate",
                                         process = "intermediate",
                                         good_out = "X004|Brewers_grain",
                                         report_as = "brewers_grain1",
                                         residual = "alcoholloss",
                                         extraction_quantity = "max",
                                         extraction_attribute = "dm",
                                         prod_attributes = prod_attributes)
    } 
    
    invisible(lapply(beercereals, .add_beercereals))
    
    return(object)
  }
  
  # processing of sugar cane and sugar beet (processed) to sugar1, molasses1 and refiningloss
  .sugar_processing <- function(object) {
    object <- .processing_global(object = object,
                                 goods_in = c("2536|Sugar cane", "2537|Sugar beet"),
                                 from = "processed",
                                 process = "refining",
                                 goods_out = c("2818|Sugar, Refined Equiv", "2544|Molasses"),
                                 report_as = c("sugar1", "molasses1"),
                                 residual = "refiningloss")
    
    object <- .processing_global(object = object,
                                 goods_in = c("2514|Maize and products"),
                                 from = "processed",
                                 process = "refining",
                                 goods_out = c("2543|Sweeteners, Other"),
                                 report_as = c("sugar1"),
                                 residual = "refiningloss")
    
    return(object)
  }
  
  # processing of oil and oilcake from palm/palmkernel (processed)
  .oilpalm_processing <- function(object) {
    # aggregate FAO products relating to oilpalm to a single raw product
    newproduct <- dimSums(object[, , list("production", c("2577|Palm Oil", "2576|Palmkernel Oil", "2595|Palmkernel Cake"), "dm")], dim = c("ItemCodeItem", "ElementShort", "attributes"))
    newproduct <- prod_attributes[, , "X003|Palmoil_Kerneloil_Kernelcake"] * newproduct
    object[, , list("X003|Palmoil_Kerneloil_Kernelcake", c("production", "domestic_supply", "processed"))] <- newproduct
    
    # extract oil
    object <- .processing_global(object = object,
                                 goods_in = "X003|Palmoil_Kerneloil_Kernelcake",
                                 from = "processed",
                                 process = "extracting",
                                 goods_out = c("2577|Palm Oil", "2576|Palmkernel Oil"),
                                 report_as = c("oil1", "oil2"),
                                 residual = "intermediate")
    
    object <- .extract_good_from_flow(object = object,
                                      good_in = "X003|Palmoil_Kerneloil_Kernelcake",
                                      from = "intermediate",
                                      process = "intermediate",
                                      good_out = "2595|Palmkernel Cake",
                                      report_as = "oilcakes1",
                                      residual = "extractionloss",
                                      extraction_quantity = "max",
                                      extraction_attribute = "dm",
                                      prod_attributes = prod_attributes)
    
    return(object)
  }
  
  # extraction of oil and oilcakes from oilcrops (processed)
  .oil_processing <- function(object) {
    # orders must match!
    crops_in <- c("2555|Soyabeans", "2820|Groundnuts (in Shell Eq)", "2557|Sunflower seed", 
                  "2559|Cottonseed", "2558|Rape and Mustardseed", "2560|Coconuts - Incl Copra",
                  "2561|Sesame seed")
    oil_out  <- c("2571|Soyabean Oil", "2572|Groundnut Oil", "2573|Sunflowerseed Oil",
                  "2575|Cottonseed Oil", "2574|Rape and Mustard Oil", "2578|Coconut Oil", 
                  "2579|Sesameseed Oil")
    cake_out <- c("2590|Soyabean Cake", "2591|Groundnut Cake", "2592|Sunflowerseed Cake", 
                  "2594|Cottonseed Cake", "2593|Rape and Mustard Cake", "2596|Copra Cake", 
                  "2597|Sesameseed Cake")   
    
    other_crops_in <- c("2570|Oilcrops, Other", "2563|Olives (including preserved)")
    other_oil_out  <- "2586|Oilcrops Oil, Other"
    other_cake_out <-  "2598|Oilseed Cakes, Other"
    
    # main oil crops
    .extract_oil <- function(j){
      object <<- .processing_global(object = object,
                                    goods_in = crops_in[j],
                                    from = "processed",
                                    process = "extracting",
                                    goods_out = oil_out[j],
                                    report_as = "oil1",
                                    residual = "intermediate")
      object <<- .extract_good_from_flow(object = object,
                                         good_in = crops_in[j],
                                         from = "intermediate",
                                         process = "intermediate",
                                         good_out = cake_out[j],
                                         report_as = "oilcakes1",
                                         residual = "extractionloss",
                                         extraction_quantity = "max",
                                         extraction_attribute = "dm",
                                         prod_attributes = prod_attributes)
    }
    
    invisible(lapply(1:length(crops_in), .extract_oil))
    
    # other oil crops
    object <- .processing_global(object = object,
                                 goods_in = other_crops_in,
                                 from = "processed",
                                 process = "extracting",
                                 goods_out = other_oil_out,
                                 report_as = "oil1",
                                 residual = "intermediate")
    
    .calc_goods_in <- function(good_in) {
      object <<- .extract_good_from_flow(object = object,
                                         good_in = good_in,
                                         from = "intermediate",
                                         process = "intermediate",
                                         good_out = other_cake_out,
                                         report_as = "oilcakes1",
                                         residual = "extractionloss",
                                         extraction_quantity = "max",
                                         extraction_attribute = "dm",
                                         prod_attributes = prod_attributes)
    }
    
    invisible(lapply(other_crops_in, .calc_goods_in))
    
    return(object)
  }
  
  # main function combining all processing functions
  .massbalance <- function(years) {
    # preparing dataset for given years
    names_processing <- c(
      "production_estimated",
      "milling", "brans1", "branoil1", "flour1",
      "refining", "sugar1", "molasses1", "refiningloss",
      "extracting", "oil1", "oil2", "oilcakes1", "extractionloss",
      "fermentation", "alcohol1", "alcohol2", "alcohol3", "brewers_grain1", "alcoholloss",
      "distilling", "ethanol1", "distillers_grain1", "distillingloss",
      "intermediate",
      "households"
    )

    cells <- getCells(CBC)
    s1 <- getNames(CBC, dim = 1)
    s2 <- c(getNames(CBC, dim = 2), names_processing)
    s3 <- attribute_types
    CBCflows <- array(dim = c(length(cells), length(years), length(s1), length(s2), length(s3)), dimnames = list(cells, years, s1, s2, s3))
    CBCflows <- as.magpie(CBCflows)
    getSets(CBCflows) <- c("region", "year", "ItemCodeItem", "ElementShort", "attributes")

    CBCflows[, , getNames(CBC, dim = 2)] <- CBC[, years, ] * attributes_wm

    CBCflows[, , list("households", "ge")] <- CBC[, years, "food_supply_kcal"] * 4.184 # conversion from 10^12 kcal to PJ
    CBCflows[, , list("households", "nr")] <- CBC[, years, "protein_supply"] / 6.25 # conversion of protein to nitrogen using average N content
    CBCflows[, , list("households", "wm")] <- CBC[, years, "food_supply"]

    CBCflows <- CBCflows[, , setdiff(getNames(CBCflows, dim = "ElementShort"), c("food_supply_kcal", "protein_supply", "food_supply", "fat_supply"))]

    CBCflows[is.na(CBCflows)]  <- 0
    CBCflows[is.nan(CBCflows)] <- 0

    # Food processing calculations
    CBCflows <- .cereal_milling_global(CBCflows)
    CBCflows <- .ethanol_processing(CBCflows)
    CBCflows <- .beer_processing(CBCflows)
    CBCflows <- .sugar_processing(CBCflows)
    CBCflows <- .oilpalm_processing(CBCflows)
    CBCflows <- .oil_processing(CBCflows)
    
    # harmonizing conversion factors within the rapeseed group
    goods_in  <- list("2558|Rape and Mustardseed", "2560|Coconuts - Incl Copra", "2561|Sesame seed", c("2570|Oilcrops, Other", "2563|Olives (including preserved)"))
    oils_out  <- list("2574|Rape and Mustard Oil", "2578|Coconut Oil", "2579|Sesameseed Oil", "2586|Oilcrops Oil, Other")
    cakes_out <- list("2593|Rape and Mustard Cake", "2596|Copra Cake", "2597|Sesameseed Cake","2598|Oilseed Cakes, Other")
    
    .harmonize1 <- function(from) {
      factor <- dimSums(CBCflows[, , list(unlist(goods_in), from)], dim = c(1, 3.1, 3.2)) / dimSums(CBCflows[, , list(unlist(goods_in), "extracting")], dim = c(1, 3.1, 3.2))
      CBCflows[, , list(unlist(goods_in), from)] <<- dimSums(CBCflows[, , list(unlist(goods_in), "extracting")], dim = 3.2) * factor
    }
    
    invisible(lapply(c("oil1", "oilcakes1", "extractionloss"), .harmonize1))
    
    .harmonize2 <- function(j) {
      CBCflows[, , list(oils_out[[j]], "production_estimated")]  <<- dimSums(CBCflows[, , list(goods_in[[j]], "oil1")], dim = c(3.1, 3.2))
      CBCflows[, , list(cakes_out[[j]], "production_estimated")] <<- dimSums(CBCflows[, , list(goods_in[[j]], "oilcakes1")], dim = c(3.1, 3.2))
    }
    
    invisible(lapply(1:length(oils_out), .harmonize2))
    
    # Alcohol production  
    crops_alcohol <- relationmatrix[relationmatrix[, 2] %in% c("others", "trce", "rice_pro", "potato", "cassav_sp", "sugar", "molasses", "brans"), 1]
    CBCflows <- .processing_global(CBCflows,
                                   goods_in  = crops_alcohol,
                                   from      = "processed",
                                   process   = "fermentation",
                                   goods_out = c("2655|Wine", "2657|Beverages, Fermented", "2658|Beverages, Alcoholic"),
                                   report_as = c("alcohol1", "alcohol2", "alcohol3"),
                                   residual  = "alcoholloss")
    
    return(CBCflows)
  }
  
  #### Calculations #### 
  
  # determine years
  if (is.null(years)) {
    years <- getYears(CBC) 
  }
  if (nchar(years[[1]]) < 5) {
    years <- paste0("y", years)
  }
  
  # estimate processing flows
  current_limit <- getOption("magclass_sizeLimit")
  on.exit(options(magclass_sizeLimit = current_limit))
  options(magclass_sizeLimit = 2e8) # in case all years of CBC are calculated this is needed
  
  massbalance <- .massbalance(years)
  
  # Define use of products that are not existing in FAOSTAT
  goods <- c("X002|Distillers_grain", "X004|Brewers_grain") 
  massbalance[, , list(goods, c("production", "domestic_supply", "feed"))]  <- collapseNames(massbalance[, , list(goods, "production_estimated")], collapsedim = 2) 
  massbalance[, , list("X001|Ethanol", c("production", "domestic_supply", "other_util"))] <- collapseNames(massbalance[, , list("X001|Ethanol", "production_estimated")], collapsedim = 2)
  
  # add remaining 'processed' to 'other_util'
  massbalance[, , "other_util"] <- dimSums(massbalance[, , c("other_util", "processed")], dim = 3.2)
  
  # remove obsolete columns
  massbalance <- massbalance[, , c("processed", "intermediate"), invert = TRUE]
  
  # map to MAgPIE categories
  massbalance <- toolAggregate(x = massbalance,
                               rel = relationmatrix,
                               dim = 3.1,
                               from = "FoodBalanceItem",
                               to = "k",
                               partrel = TRUE)
  
  #### Return ####
  return(list(x = massbalance,
              weight = NULL,
              unit = "MT C, Mt DM, PJ, Mt K, Mt Nr, Mt P, Mt WM",
              description = "FAO massbalance calculates all conversion processes within the FAO CBS/FBS and makes them explict. More complete version can be found in calcFAOmassbalance"))
}