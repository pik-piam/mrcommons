#' @title calcFAOmassbalance_pre
#' @description Calculates an extended version of the Food Balance Sheets. Makes explicit the conversion processes that convert one type of product into another. 
#' Includes processes like milling, distilling, extraction etc. Adds certain byproducts like distillers grains or ethanol.
#'
#' @param years years to be estimated. Takes a lot of time.
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

calcFAOmassbalance_pre2 <- function(years = paste0("y",(seq(1965,2010,5)))) {
  #### Data input
  ## FAO Commodity Balance  ------
  CBC <- calcOutput(type = "FAOharmonized", aggregate = F)
  
  getSets(CBC) <- c("region", "year", "ItemCodeItem.ElementShort")
  if (any(duplicated(dimnames(CBC)[[3]]) == T)) {
    stop("The folowing dimnames are duplicated: ", paste(dimnames(CBC)[[3]][which(duplicated(dimnames(CBC)[[3]]) == T)], collapse = "\", \""))  
  }
  
  # remove double counting  
  removethem <- c("2903|Vegetal Products + (Total)",
                  "2905|Cereals - Excluding Beer + (Total)",
                  "2907|Starchy Roots + (Total)",
                  "2908|Sugar Crops + (Total)",
                  "2909|Sugar & Sweeteners + (Total)",
                  "2911|Pulses + (Total)",
                  "2912|Treenuts + (Total)",
                  "2913|Oilcrops + (Total)",
                  "2914|Vegetable Oils + (Total)",
                  "2918|Vegetables + (Total)",
                  "2919|Fruits - Excluding Wine + (Total)",
                  "2922|Stimulants + (Total)",
                  "2923|Spices + (Total)",
                  "2924|Alcoholic Beverages + (Total)",
                  "2928|Miscellaneous + (Total)",
                  "2941|Animal Products + (Total)",
                  "2943|Meat + (Total)",
                  "2945|Offals + (Total)",
                  "2946|Animal fats + (Total)",
                  "2948|Milk - Excluding Butter + (Total)",
                  "2949|Eggs + (Total)",
                  "2960|Fish, Seafood + (Total)",
                  "2961|Aquatic Products, Other + (Total)",
                  "2805|Rice (Milled Equivalent)",
                  "2556|Groundnuts (Shelled Eq)",
                  "2827|Sugar, Raw Equivalent",
                  "2542|Sugar (Raw Equivalent)",
                  "2815|Roots & Tuber Dry Equiv",
                  "2562|Palm kernels",
                  "2901|Grand Total + (Total)",
                  "2747|Silk",
                  "2739|Milk, Skimmed",
                  "2738|Milk, Whole",
                  "2741|Cheese",
                  "2672|Rubber",
                  "2742|Whey",
                  "2671|Tobacco")
  
  CBC <- CBC[, , removethem, invert = T]
  
  CBC <- complete_magpie(CBC, fill = 0)
  
  missingproducts <- c("X001|Ethanol", 
                       "X002|Distillers_grain", 
                       "X003|Palmoil_Kerneloil_Kernelcake", 
                       "X004|Brewers_grain")
  
  CBC <- add_columns(CBC, addnm = missingproducts, dim = 3.1)
  
  
  ## Production Attributes  ------
  prod_attributes <- calcOutput("Attributes", aggregate = F)
  remove_prod <-  c("betr", "begr", "pasture", "scp", "res_cereals", "res_fibrous", "res_nonfibrous", "wood", "woodfuel")
  prod_attributes <- prod_attributes[, , remove_prod, invert = T]
  
  ## Sectoral mapping for FAO items ------
  relationmatrix <- toolGetMapping("FAOitems.csv", type = "sectoral", where = "mappingfolder")
  relationmatrix <- relationmatrix[, c("FoodBalanceItem", "k")]
  relationmatrix <- relationmatrix[!duplicated(relationmatrix[, "FoodBalanceItem"]), ]
  
  other_crops <- relationmatrix[relationmatrix[, 2] == "others", 1]
  tece <- relationmatrix[relationmatrix[, 2] == "tece", 1]
  trce <- relationmatrix[relationmatrix[, 2] == "trce", 1]
  potato <- relationmatrix[relationmatrix[, 2] == "potato", 1]
  cassava_sp <- relationmatrix[relationmatrix[, 2] == "cassav_sp", 1]
  sugar <- relationmatrix[relationmatrix[, 2] == "sugar", 1]
  molasse <- relationmatrix[relationmatrix[, 2] == "molasses", 1]
  brans <- relationmatrix[relationmatrix[, 2] == "brans", 1]
  
  
  ## Map production attributes to FAO items ------ 
  
  prod_attributes <- toolAggregate(x = prod_attributes, rel = relationmatrix, dim = 3.2, from = "k", to = "FoodBalanceItem", partrel = TRUE)
  getSets(prod_attributes) <- c("region", "year", "attributes", "ItemCodeItem")
  # change prod attributes from share of DM to share of WM
  attributes_wm <- (prod_attributes / dimSums(prod_attributes[, , "wm"], dim = "attributes"))
  
  if (!(all(getNames(CBC, dim = "ItemCodeItem") %in% getNames(attributes_wm, dim = "ItemCodeItem")))) {
    vcat(verbosity = 2, "The following items were removed from the dataset because of missing prod_attributes: ", paste(getNames(CBC, dim = 1)[!(getNames(CBC, dim = 1) %in% getNames(attributes_wm, dim = 2))], collapse = "\", \""))
    CBC <- CBC[, , getNames(CBC, dim = "ItemCodeItem")[which(getNames(CBC, dim = "ItemCodeItem") %in% getNames(attributes_wm, dim = "ItemCodeItem"))]]
  }
  if (!all(getNames(attributes_wm, dim = "ItemCodeItem") %in% getNames(CBC, dim = "ItemCodeItem"))) {
    stop("For the following items there were entries in prod_attributes but no respective data: ", paste(getNames(attributes_wm, dim = 2)[!(getNames(attributes_wm, dim = 2) %in% getNames(CBC, dim = 1))], collapse = "\", \""))
  }
  
  
  
  #### Definition of Subfunctions #####
  
  .processing_global <- function(object,
                                 goods_in = c("2536|Sugar cane", "2537|Sugar beet"),
                                 from = "processed",
                                 process = "refining",
                                 # the order matters!
                                 goods_out = c("2818|Sugar, Refined Equiv", "2544|Molasses"),
                                 report_as = c("sugar1", "molasses1"),
                                 lossname = "refiningloss") {
      if (any(object[, , goods_in][, , c(report_as, lossname)] != 0)) {
        stop("Output flows already exist.")
      }
      if (any(object[, , goods_out][, , c("production_estimated")] != 0)) {
        stop("Output flows already exist.")
      }
    
      # attributes relevant for checking massbalance and conv_factor
      relevant_attributes <- c("dm", "nr", "p", "k", "ge")
      
      conv_factor <- (dimSums(object[, , list(goods_out, "production")], dim = c("region", "ElementShort"))
                          / dimSums(object[, , list(goods_in, from)], dim = c("region", "ItemCodeItem", "ElementShort")))
      
      if (any(dimSums(conv_factor[, , goods_out], dim = "ItemCodeItem")[, , relevant_attributes] > 1)) {
        stop("conversion factors exceed 1. not suitable for a global conversion factor.")
      }
      
      # estimate outputs
      .estim_outputs <- function(j) {
        object[, , list(report_as[j], goods_in)] <<- dimSums(object[, , list(goods_in, from)], dim = "ElementShort") * conv_factor[, , goods_out[j], drop = T]
        object[, , list("production_estimated", goods_out[j])] <<- dimSums(object[, , list(report_as[j], goods_in)], dim = c("ElementShort", "ItemCodeItem"))
      }
      
      invisible(lapply(c(1:length(goods_out)), .estim_outputs))
      
      # calculate refining losses as mass balance difference
      object[, , list(goods_in, lossname)] <- (dimSums(object[, , list(goods_in, from)], dim = c("ElementShort"))
                                               - dimSums(object[, , list(goods_in, report_as)], dim = c("ElementShort")))
      
      # Massbalance tests ----
      diff <- (dimSums(object[, , goods_in][, , c(report_as, lossname)], dim = c("ElementShort"))
               - dimSums(object[, , goods_in][, , c(from)], dim = c("ElementShort")))
      if (any(abs(diff) > 0.01)) {
        print(diff[, , goods_in])
        stop("NAs in dataset or function corrupt.")
      }
      
      diff <- (dimSums(object[, , goods_in][, , c(from)], dim = c("ElementShort", "ItemCodeItem"))
               - dimSums(object[, , goods_in][, , c(lossname)], dim = c("ElementShort", "ItemCodeItem"))
               - dimSums(object[, , goods_out][, , c("production_estimated")], dim = c("ElementShort", "ItemCodeItem")))
      if (any(abs(diff) > 0.01)) {
        print(diff[, , goods_in])
        stop("NAs in dataset or function corrupt.")
      }
      
      diff <- sum(object[, , goods_out][, , c("production_estimated")]) - sum(object[, , goods_out][, , c("production")])
      if (any(abs(diff) > 0.01)) {
        print(diff[, , goods_in])
        stop("global estimated production does not meet global production")
      }
      
      # negative value check
      if (any(object[, , goods_in][, , c(report_as, from, lossname)][, , relevant_attributes] < 0)) {
        print(diff[, , goods_in])
        warning("Massbalancing failed, negative values.")
      }
      
      # return ----
      if (from != process) {
        object[, , list(process, goods_in)] <- object[, , list(from, goods_in)]
        object[, , list(from, goods_in)] <- 0
      }
      return(object)
    }
  
 
  .extract_good_from_flow <- function(object,
                                      good_in,
                                      from,
                                      process,
                                      extract,
                                      report_as,
                                      extraction_quantity,
                                      extraction_attribute,
                                      residual,
                                      prod_attributes) {
    if (length(from) > 1) {
      stop("please only use one item as \"from\"")
    }
    if (length(report_as) > 1) {
      stop("please only use one item as \"report_as\"")
    }
    if (length(good_in) > 1) {
      stop("please only use one item as \"good_in\"")
    }
    if (length(extract) > 1) {
      stop("please only use one item as \"extract\"")
    }
    if (any(object[, , good_in][, , c(report_as, residual)] != 0)) {
      warning("output flows already exist")
    }
    
    attr_no_wm <- c("dm", "nr", "p", "k", "ge") # DL also no c?
    
    # given globally as share of extraction attribute
    attributes_from <- dimSums(object[, , list(from, good_in), drop = T], dim = "region") / dimSums(object[, , list(from, good_in, extraction_attribute), drop = T], dim = c("region"))
    attributes_to <- prod_attributes[, , extract, drop = T] / prod_attributes[, , list(extract, extraction_attribute), drop = T]
    
    extraction_factor <- attributes_from[, , attr_no_wm] / attributes_to[, , attr_no_wm]
    
    maxextract <- as.magpie(apply(X = extraction_factor, MARGIN = 2, FUN = min))
    
    if (extraction_quantity == "max") {
      extraction_quantity <- maxextract
    } else if (any(extraction_quantity > maxextract)) {
      print((extraction_quantity > maxextract))
      stop("too high extraction quantity")
    }
    
    # DL extracted e.g. ethanol quantity (maximum amount of ethanol that can be produce without violating the available wheat.other_util in any of it's attributes)
    # if the extraction_quantity is the original input extraction_quantity, isn't that then basically the constraint regarding weight, and shouldn't then
    # extracted be the full dm? 
    extracted <- object[, , list(from, good_in, extraction_attribute), drop = T] * extraction_quantity * attributes_to
    losses <- dimSums(object[, , list(good_in, from)], dim = "ElementShort") - extracted
    
    object[, , list(good_in, report_as)] <- extracted
    object[, , list(good_in, residual)] <- losses
    
    object[, , list("production_estimated", extract)] <- object[, , list("production_estimated", extract)] + extracted
    
    # massbalance check
    diff <- (dimSums(object[, , list(good_in, c(report_as, residual))], dim = c("ElementShort"))
             - dimSums(object[, , list(good_in, from)], dim = c("ElementShort")))
    if (any(abs(diff) > 0.01)) {
      print(diff[, , good_in])
      stop("NAs in dataset or function corrupt.")
    }
    
    # DL it seems that if from == process, it is from == process == intermediate, and 
    # it is set to zero manually after the function anyways??
    if (from != process) {
      object[, , list(process, good_in)] <- object[, , list(from, good_in)]
      object[, , list(from, good_in)] <- 0
    }
    return(object)
  }
  
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
    brans <- "2600|Brans"
    milled <- "food"
    flour <- "flour1"
    process <- "milling"
    
    if (any(object[, , cereals][, , c(flour, "brans1", "branoil1")] != 0)) {
      stop("Output flows already exist.")
    }
    
    milled_global <- dimSums(object[, , list(cereals, milled)], dim = c("region", "ElementShort"))
    brans_global <- dimSums(object[, , list(brans, "production")], dim = c("region", "ElementShort", "ItemCodeItem"))
    # as share of dm instaed of wm
    bran_attributes <- setYears(brans_global / dimSums(brans_global[, , "dm"], dim = "attributes"), NULL)
    
    # estimating bran based on simple factors (Feedipedia)
    # rice: 10%, wheat: 25%
    # we use 20% for wheat to account for some wholegrain meal
    # own estimates to not violate massbalance: corn and trce get only 5%
    bran_ratio <- new.magpie("GLO", NULL, cereals, fill = 0.20)
    getSets(bran_ratio) <- c("region", "year", "ItemCodeItem")
    bran_ratio[, , c("2804|Rice (Paddy Equivalent)", "2514|Maize and products")] <- 0.1
    bran_ratio[, , c("2518|Sorghum and products", "2517|Millet and products")] <- 0.05
    brans_uncalibrated <- dimSums(bran_ratio * milled_global[, , "dm"], dim = "ItemCodeItem")
    bran_ratio <- bran_ratio * dimSums(brans_global[, , "dm"], dim = "attributes") / brans_uncalibrated
    
    bran_estimated <- bran_ratio * dimSums(object[, , list(cereals, milled)][, , "dm"], dim = "attributes") * bran_attributes
    object[, , list("brans1", cereals)] <- dimSums(bran_estimated[, , cereals], dim = c("ElementShort"))
    object[, , list("production_estimated", brans)] <- dimSums(bran_estimated[, , cereals], dim = c("ItemCodeItem", "ElementShort"))
    object[, , list(flour, cereals)] <- object[, , list(milled, cereals)] - bran_estimated
    
    .branoil1_production <- function(object, branoilItem, cropItem) {
      branoil_ratio <- (dimSums(object[, , list(branoilItem, "production")], dim = c("region", "ItemCodeItem", "ElementShort"))  
                         / dimSums(milled_global[, , cropItem], dim = "ItemCodeItem"))
      estimated_branoil <- object[, , list(cropItem, milled)] * branoil_ratio
      object[, , list("branoil1", cropItem)] <- dimSums(estimated_branoil[, , cropItem], dim = c("ElementShort"))
      object[, , list("production_estimated", branoilItem)] <- dimSums(estimated_branoil[, , cropItem], dim = c("ItemCodeItem", "ElementShort"))
      object[, , list(flour, cropItem)] <- object[, , list(flour, cropItem)] - estimated_branoil
      return(object)
    }
    
    object <- .branoil1_production(object, "2582|Maize Germ Oil", "2514|Maize and products")
    object <- .branoil1_production(object, "2581|Ricebran Oil", "2804|Rice (Paddy Equivalent)")
    
    # massbalance checks
    diff <- (dimSums(object[, , cereals][, , c(flour, "brans1", "branoil1")], dim = c("ElementShort"))
             - dimSums(object[, , cereals][, , c(milled)], dim = c("ElementShort")))
    if (any(round(diff, 5) != 0)) {
      print(diff[, , cereals])
      stop("NAs in dataset or function corrupt.1")
    }
    diff <- (dimSums(object[, , cereals][, , c("branoil1")], dim = c("ElementShort", "ItemCodeItem"))
             - dimSums(object[, , c("2581|Ricebran Oil", "2582|Maize Germ Oil")][, , c("production_estimated")], dim = c("ElementShort", "ItemCodeItem")))
    if (any(round(diff, 5) != 0)) {
      print(diff)
      stop("NAs in dataset or function corrupt.2")
    }
    
    # negative value check
    if (any(round(object[, , cereals][, , c(flour, milled, "brans1", "branoil1")], 5) < 0)) {
      stop("Massbalancing failed, negative values.")
    }
    
    # add processed item
    object[, , list(process, cereals)] <- object[, , list(milled, cereals)]
    object[, , list(milled, cereals)] <- 0
    
    ### Fooduse in brans is included in the commodity balance sheets, but not reflected in calories.
    # We subtract bran consumption from cereal consumption in the respective countries.
    # For simplicity, we distribute brans proportional to all cereal fooduse.
    
    # DL this changes only household dimension -> could be treated separately?
    branshr <- (dimSums(object[, , list(brans, milled, c("wm", "ge", "nr"))], dim = c(3.1, 3.2)) 
                  / dimSums(object[, , list(cereals, "households", c("wm", "ge", "nr"))], dim = c(3.1, 3.2)))
    branshr[is.nan(branshr)] <- 0
    if (any(branshr < 0)) {
      vcat(1, "branshr should not be smaller than zero.")
    }
    object[, , list(cereals, "households", c("wm", "ge", "nr"))] <- (1 - branshr) * object[, , list(cereals, "households", c("wm", "ge", "nr"))]
    object[, , list(brans, "households", c("wm", "ge", "nr"))] <- object[, , list(brans, milled, c("wm", "ge", "nr"))]
    
    return(object)
  }
  
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
    
    # Wheat instead of tece would be more correct, but we need to have homogenous products
    tece_maize <- c(tece, "2514|Maize and products")
    object[, , list(tece_maize, "intermediate")] <- 0
    
    # liter yield in dm
    ethanol_yield_liter_per_ton_tece <- 375
    ethanol_yield_liter_per_ton_maize <- 408
    ethanol_yield_liter_per_ton_sugarcane <- 654
    
    ethanol_yield_liter_per_ton_tece_maize <- c(rep(ethanol_yield_liter_per_ton_tece, length(tece)), ethanol_yield_liter_per_ton_maize)
    extraction_quantity_tece_maize <- 0.789 * ethanol_yield_liter_per_ton_tece_maize / 1000
    extraction_quantity_sugarcane <- 0.789 * ethanol_yield_liter_per_ton_sugarcane / 1000

    # ethanol processing from tece and maize (ethanol1, distilles_grain, and distillingloss)
    .extract_ethanol <- function(j) {
      object <<- .extract_good_from_flow(object = object,
                                         good_in = tece_maize[j],
                                         from = "other_util",
                                         process = "distilling",
                                         extract = "X001|Ethanol",
                                         report_as = "ethanol1",
                                         extraction_quantity = extraction_quantity_tece_maize[j],
                                         extraction_attribute = "dm",
                                         residual = "intermediate",
                                         prod_attributes = prod_attributes)
      
      object <<- .extract_good_from_flow(object = object,
                                         good_in = tece_maize[j],
                                         from = "intermediate",
                                         process = "intermediate",
                                         extract = "X002|Distillers_grain",
                                         report_as = "distillers_grain1",
                                         extraction_quantity = "max",
                                         extraction_attribute = "nr",
                                         residual = "distillingloss",
                                         prod_attributes = prod_attributes) 
    }
    invisible(lapply(1:length(tece_maize), .extract_ethanol))
    
    object[, , list(tece_maize, "intermediate")] <- 0
    
    # ethanol processing from sugarcane (only ethanol1 and distillingloss)
    object <- .extract_good_from_flow(object = object,
                                      good_in = "2536|Sugar cane",
                                      from = "other_util",
                                      process = "distilling",
                                      extract = "X001|Ethanol",
                                      report_as = "ethanol1",
                                      extraction_quantity = extraction_quantity_sugarcane,
                                      extraction_attribute = "dm",
                                      residual = "distillingloss",
                                      prod_attributes = prod_attributes)
   
    return(object)
  }
  
  .beer_processing <- function(object) {
    # Barley would be more correct, but we need to have homogenous products
    beercereals <- tece
    
    object <- .processing_global(object = object,
                                 goods_in = beercereals,
                                 from = "processed",
                                 process = "fermentation",
                                 goods_out = "2656|Beer",
                                 report_as = "alcohol1",
                                 lossname = "intermediate")
    
    .add_beercereals <- function(x) {
      object <<- .extract_good_from_flow(object = object,
                                         good_in = x,
                                         from = "intermediate",
                                         process = "intermediate",
                                         extract = "X004|Brewers_grain",
                                         report_as = "brewers_grain1",
                                         extraction_quantity = "max",
                                         extraction_attribute = "dm",
                                         residual = "alcoholloss",
                                         prod_attributes = prod_attributes)
    } 
    
    invisible(lapply(beercereals, .add_beercereals))
    
    object[, , "intermediate"] <- 0
    return(object)
  }
  
  .sugar_processing <- function(object) {
    object <- .processing_global(object = object,
                                 goods_in = c("2536|Sugar cane", "2537|Sugar beet"),
                                 from = "processed",
                                 process = "refining",
                                 # the order matters!
                                 goods_out = c("2818|Sugar, Refined Equiv", "2544|Molasses"),
                                 report_as = c("sugar1", "molasses1"),
                                 lossname = "refiningloss")
    
    object <- .processing_global(object = object,
                                 goods_in = c("2514|Maize and products"),
                                 from = "processed",
                                 process = "refining",
                                 # the order matters!
                                 goods_out = c("2543|Sweeteners, Other"),
                                 report_as = c("sugar1"),
                                 lossname = "refiningloss")
    
    return(object)
  }
  
  .oilpalm_processing <- function(object, prod_attributes) {
    newproduct <- dimSums(object[, , list("production", c("2577|Palm Oil", "2576|Palmkernel Oil", "2595|Palmkernel Cake"), "dm")], dim = c("ItemCodeItem", "ElementShort", "attributes"))
    newproduct <- prod_attributes[, , "X003|Palmoil_Kerneloil_Kernelcake"] * newproduct
    object[, , list("X003|Palmoil_Kerneloil_Kernelcake", c("production", "domestic_supply", "processed"))] <- newproduct
    object[, , list("X003|Palmoil_Kerneloil_Kernelcake", "intermediate")] <- 0
    
    # extract oil
    object <- .processing_global(object = object,
                                 goods_in = "X003|Palmoil_Kerneloil_Kernelcake",
                                 from = "processed",
                                 process = "extracting",
                                 # the order matters!
                                 goods_out = c("2577|Palm Oil", "2576|Palmkernel Oil"),
                                 report_as = c("oil1", "oil2"),
                                 lossname = "intermediate")
    
    object <- .extract_good_from_flow(object = object,
                                      good_in = "X003|Palmoil_Kerneloil_Kernelcake",
                                      from = "intermediate",
                                      process = "intermediate",
                                      extract = "2595|Palmkernel Cake",
                                      report_as = "oilcakes1",
                                      extraction_quantity = "max",
                                      extraction_attribute = "dm",
                                      residual = "extractionloss",
                                      prod_attributes = prod_attributes)
    
    object[, , "intermediate"] <- 0
    return(object)
  }
  
  .oil_processing <- function(object) {
    
    # orders must match!
    goods_in <- c("2555|Soyabeans", "2820|Groundnuts (in Shell Eq)", "2557|Sunflower seed", 
                  "2559|Cottonseed", "2558|Rape and Mustardseed", "2560|Coconuts - Incl Copra",
                  "2561|Sesame seed")
    oil_out <- c("2571|Soyabean Oil", "2572|Groundnut Oil", "2573|Sunflowerseed Oil",
                "2575|Cottonseed Oil", "2574|Rape and Mustard Oil", "2578|Coconut Oil", 
                "2579|Sesameseed Oil")
    cake_out <- c("2590|Soyabean Cake", "2591|Groundnut Cake", "2592|Sunflowerseed Cake", 
                 "2594|Cottonseed Cake", "2593|Rape and Mustard Cake", "2596|Copra Cake", 
                 "2597|Sesameseed Cake")   
    
    other_goods_in <- c("2570|Oilcrops, Other", "2563|Olives (including preserved)")
    other_oil_out <- "2586|Oilcrops Oil, Other"
    other_cake_out <-  "2598|Oilseed Cakes, Other"
    
    object[, , list(c(goods_in, other_goods_in), "intermediate")] <- 0
    
    # main oil crops
    .extract_oil <- function(j){
      object <<- .processing_global(object = object,
                                    goods_in = goods_in[j],
                                    from = "processed",
                                    process = "extracting",
                                    # the order matters!
                                    goods_out = oil_out[j],
                                    report_as = "oil1",
                                    lossname = "intermediate")
      object <<- .extract_good_from_flow(object = object,
                                         good_in = goods_in[j],
                                         from = "intermediate",
                                         process = "intermediate",
                                         extract = cake_out[j],
                                         report_as = "oilcakes1",
                                         extraction_quantity = "max",
                                         extraction_attribute = "dm",
                                         residual = "extractionloss",
                                         prod_attributes = prod_attributes)
    }
    
    invisible(lapply(1:length(goods_in), .extract_oil))
    
    # other oil crops
    object <- .processing_global(object = object,
                                 goods_in = other_goods_in,
                                 from = "processed",
                                 process = "extracting",
                                 # the order matters!
                                 goods_out = other_oil_out,
                                 report_as = "oil1",
                                 lossname = "intermediate")
    
    .calc_goods_in <- function(good_in) {
      object <<- .extract_good_from_flow(object = object,
                                         good_in = good_in,
                                         from = "intermediate",
                                         process = "intermediate",
                                         extract = other_cake_out,
                                         report_as = "oilcakes1",
                                         extraction_quantity = "max",
                                         extraction_attribute = "dm",
                                         residual = "extractionloss",
                                         prod_attributes = prod_attributes)
    }
    
    invisible(lapply(other_goods_in, .calc_goods_in))
    
    object[, , list(c(goods_in, other_goods_in), "intermediate")] <- 0
    return(object)
  }
  
  # main function combining all processing functions
  .massbalance_in_yeargroups <- function(year_x) {
    
    ###### preparing dataset for given year
    CBCflows <- CBC[, year_x, ] * attributes_wm
    
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
    
    CBCflows <- add_columns(CBCflows, addnm = names_processing, dim = 3.2)
    
    CBCflows[, , list("households", "ge")] <- CBC[, year_x, "food_supply_kcal"] * 4.184
    CBCflows[, , list("households", "nr")] <- CBC[, year_x, "protein_supply"] / 6.25
    CBCflows[, , list("households", "wm")] <- CBC[, year_x, "food_supply"]
    
    CBCflows <- CBCflows[, , setdiff(getNames(CBCflows, dim = "ElementShort"), c("food_supply_kcal", "protein_supply", "food_supply", "fat_supply"))]
    
    CBCflows[is.na(CBCflows)] <- 0
    CBCflows[is.nan(CBCflows)] <- 0
    
    ###### Food processing calculations
    
    ## Cereal milling
    CBCflows <- .cereal_milling_global(CBCflows)
    
    ## Ethanol production
    CBCflows <- .ethanol_processing(CBCflows)
  
    ## Beer brewing
    CBCflows <- .beer_processing(CBCflows)
    
    ## Sugar refining
    CBCflows <- .sugar_processing(CBCflows)
    
    ## Oil milling
    CBCflows <- .oilpalm_processing(CBCflows, prod_attributes = prod_attributes)
    CBCflows <- .oil_processing(CBCflows)
  
    ## harmonizing conversion factors within the rapeseed group
    oils_out <- list("2574|Rape and Mustard Oil", "2578|Coconut Oil", "2579|Sesameseed Oil", "2586|Oilcrops Oil, Other")
    cakes_out <- list("2593|Rape and Mustard Cake", "2596|Copra Cake", "2597|Sesameseed Cake","2598|Oilseed Cakes, Other")
    goods_in <- list("2558|Rape and Mustardseed", "2560|Coconuts - Incl Copra", "2561|Sesame seed", c("2570|Oilcrops, Other", "2563|Olives (including preserved)"))
    
    .harmonize1 <- function(from) {
      factor <- dimSums(CBCflows[, , unlist(goods_in)][, , from], dim = c(1, 3.1, 3.2)) / dimSums(CBCflows[, , unlist(goods_in)][, , "extracting"], dim = c(1, 3.1, 3.2))
      CBCflows[, , unlist(goods_in)][, , from] <<- dimSums(CBCflows[, , unlist(goods_in)][, , "extracting"], dim = 3.2) * factor
    }
    
    lapply(c("oil1", "oilcakes1", "extractionloss"), .harmonize1)
    
    .harmonize2 <- function(j) {
      CBCflows[, , list(oils_out[[j]], "production_estimated")] <<- dimSums(CBCflows[, , list(goods_in[[j]], "oil1")], dim = c(3.1, 3.2))
      CBCflows[, , list(cakes_out[[j]], "production_estimated")] <<- dimSums(CBCflows[, , list(goods_in[[j]], "oilcakes1")], dim = c(3.1, 3.2))
    }
    
    lapply(1:length(oils_out), .harmonize2)
    
    ## Alcohol production
    CBCflows <- .processing_global(CBCflows,
                                  goods_in = c(other_crops, trce, "2804|Rice (Paddy Equivalent)", potato, cassava_sp, sugar, molasse, "2600|Brans"),
                                  goods_out = c("2655|Wine", "2657|Beverages, Fermented", "2658|Beverages, Alcoholic"),
                                  from = "processed",
                                  process = "fermentation",
                                  report_as = c("alcohol1", "alcohol2", "alcohol3"),
                                  lossname = "alcoholloss")
    
    return(CBCflows)
  }
  

  #### Calculations 
  ## Setting up massbalance for each considered year ------
  if (is.null(years)) {
    years <- getYears(CBC)  # DL why does this give ALL years while default input is five year steps?
  }
  if (nchar(years[[1]]) < 5) {
    years <- paste0("y", years)
  }
  
  ## estimating processing flows
  massbalance <- mbind(lapply(X = years, FUN = .massbalance_in_yeargroups))
  # massbalance <- mbind(madlapply(X = years, FUN = massbalance_in_yeargroups))
  
  ## Define use of products that are not existing in FAOSTAT  ------
  
  # DL why domestic_supply == feed for distillers grain/brewers grain?
  goods <- c("X002|Distillers_grain", "X004|Brewers_grain")
  massbalance[, , list(goods, c("production", "domestic_supply", "feed"))]  <- collapseNames(massbalance[, , list(goods, "production_estimated")], collapsedim = 2) 
  massbalance[, , list("X001|Ethanol", c("production", "domestic_supply", "other_util"))] <- collapseNames(massbalance[, , list("X001|Ethanol", "production_estimated")], collapsedim = 2)
  
  # add remaining 'processed' to 'other_util'
  massbalance[, , "other_util"] <- dimSums(massbalance[, , c("other_util", "processed")], dim = 3.2)
  
  # remove obsolete columns
  massbalance <- massbalance[, , c("processed", "intermediate"), invert = TRUE]
  
  # map to MAgPIE categories
  massbalance <- toolAggregate(x = massbalance, rel = relationmatrix, dim = 3.1, from = "FoodBalanceItem", to = "k", partrel = TRUE)
  
  #### Return ----
  return(list(
    x = massbalance,
    weight = NULL,
    unit = "Mt DM, Mt WM, PJ, Mt Nr, Mt P, Mt K",
    description = "FAO massbalance calculates all conversion processes within the FAO CBS/FBS and makes them explict. More complete version can be found in calcFAOmassbalance"
  ))
}