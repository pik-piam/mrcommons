#' @title calcPricesProducer
#' @description producer prices for agricultural products. 05USDppp/tDM
#'
#'
#'
#'
#' @param products either "kcr" or "kcl"
#' @param calculation type of calculation "FAO" (directly reads the data), "VoP"
#' calculates as VoP/Production, only "FAO" available for "kli" products.
#' @return magpie object. prices in year specific annual
#' @author Edna J. Molina Bacca
#' @importFrom magpiesets findset
#' @importFrom GDPuc convertGDP
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("PricesProducer")
#' }
#'
calcPricesProducer <- function(products = "kcr", calculation = "VoP") {

  ## Conversion of current USD MER to 05USDppp
  gdpPPP <- calcOutput("GDP", aggregate = FALSE, FiveYearSteps = FALSE)[, , "gdp_SSP2"]
  gdpMER <- readSource("WDI", "NY.GDP.MKTP.CD")
  years <- intersect(getYears(gdpPPP), getYears(gdpMER))
  gdpConMER <- setYears(gdpMER[, 2005, ] / gdpMER[, years, ], years)
  gdp <- setNames(gdpConMER * setYears(gdpPPP[, 2005, ] / gdpMER[, 2005, ], NULL), NULL)
  # Read Prices producer with FAO format

  if (products == "kcr") {
  if (calculation == "FAO") {

    # items for aggregation
    pricesProdFAO <- readSource("FAO_online", "PricesProducerAnnual") # USD per ton
    years <- intersect(getYears(pricesProdFAO), getYears(gdp))
    pricesProdFAO <- pricesProdFAO[, years, ] * gdp[, years, ] # USD per ton
    pricesProdFAO [!is.finite(pricesProdFAO)] <- 0

    getNames(pricesProdFAO)[getNames(pricesProdFAO) == "254|Oil palm fruit"] <- "254|Oil, palm fruit"
    mappingFAO <- toolGetMapping("FAO2LUH2MAG_croptypes.csv", type = "sectoral", where = "mrcommons")
    itemsIntersect <- intersect(getNames(pricesProdFAO), unique(mappingFAO$ProductionItem))

    # weight: Production
    weightProd <- collapseNames(readSource("FAO_online", "Crop")[, , "production"])
    getNames(weightProd)[getNames(weightProd) == "254|Oil palm fruit"] <- "254|Oil, palm fruit"
    names <- intersect(getNames(weightProd), getNames(pricesProdFAO[, , itemsIntersect]))
    years <- intersect(getYears(weightProd), getYears(pricesProdFAO[, , itemsIntersect]))
    mappingFAO <- mappingFAO[mappingFAO$ProductionItem %in% names, ]

    # Aggregation to magpie objects
    pricesProdFAOkcr <- toolAggregate(pricesProdFAO[, years, names], rel = mappingFAO,
                                         from = "ProductionItem", to = "kcr", weight = weightProd[, years, names],
                                         dim = 3, wdim = 3)[, , "remaining", invert = TRUE]
    missing <- setdiff(findset("kcr"), getNames(pricesProdFAOkcr))

    # Fill with maiz' value the missing crops (betr,begr,foddr)
    pricesProdFAOkcr <- add_columns(pricesProdFAOkcr, addnm = missing, dim = 3.1)
    pricesProdFAOkcr[, , missing] <- pricesProdFAOkcr[, , "maiz"]

    # output
    x <- pricesProdFAOkcr
    weight <- collapseNames(calcOutput("Production", products = "kcr", aggregate = FALSE, attributes = "dm"))

    # years and names subseting
    years <- intersect(getYears(weight), getYears(x))
    names <- intersect(getNames(weight), getNames(x))

    weight <- weight[, years, names]
    x <- x[, years, names]

  } else if (calculation == "VoP") {

    # Value of production (USD05MER -> USD05PPP)
    vop <- calcOutput("VoPcrops", aggregate = FALSE)
    vop <- convertGDP(vop, unit_in = "constant 2005 US$MER",
                           unit_out = "constant 2005 Int$PPP",
                           replace_NAs = "no_conversion")
    production <- collapseNames(calcOutput("Production", products = "kcr", aggregate = FALSE, attributes = "dm"))

    years <- intersect(getYears(production), getYears(vop))
    names <- intersect(getNames(production), getNames(vop))
    prices <- vop[, years, names] / production[, years, names]

    # categories with all NaN (begr and betr, which have no VoP and no production data)
    missing <- setdiff(findset("kcr"), where(is.finite(prices))$true$data)
    prices[, , missing] <- prices[, , "maiz"] # fill with maiz values

    # set rest of missing values to zero
    prices[!is.finite(prices)] <- 0

    weight <- production[, years, getNames(prices)]
    weight[prices == 0] <- 0
    x <- prices[, years, ]

  } else {
   stop("Type not valid")
 }

  } else if (products == "kli") {

    if (calculation == "FAO") {

      pricesProdFAO <- readSource("FAO_online", "PricesProducerAnnual") # USD per ton
      years <- intersect(getYears(pricesProdFAO), getYears(gdp))
      pricesProdFAO <- pricesProdFAO[, years, ] * gdp[, years, ] # 05USDppp per ton
      pricesProdFAO [!is.finite(pricesProdFAO)] <- 0
      mappingFAO <- toolGetMapping("FAOitems.csv", type = "sectoral", where = "mappingfolder") # Reads mapping
      itemsIntersect <- intersect(getNames(pricesProdFAO), unique(mappingFAO$ProductionItem))

      weightProd <- collapseNames(readSource("FAO", "LivePrim")[, , "production"]) # Prod. of livestock primary prod
      # subseting of items
      names <- intersect(getNames(weightProd), getNames(pricesProdFAO[, , itemsIntersect]))
      years <- intersect(getYears(weightProd), getYears(pricesProdFAO[, , itemsIntersect]))
      mappingFAO <- mappingFAO[mappingFAO$ProductionItem %in% names, ]

      # Aggregation to magpie objects
      pricesProdFAOkli <- toolAggregate(pricesProdFAO[, years, names], rel = mappingFAO, from = "ProductionItem",
                                           to = "k", weight = weightProd[, years, names], dim = 3, wdim = 3)

      # weight setup
      weight <- collapseNames(calcOutput("Production", products = "kli", aggregate = FALSE, attributes = "dm"))
      x <- pricesProdFAOkli

      years <- intersect(getYears(weight), getYears(x))
      names <- intersect(getNames(weight), getNames(x))

      weight <- weight[, years, names]
      x <- x[, years, names]

    } else {
      stop("Type not valid")
    }
  }

  units <- "05USDppp/tDM"

  return(list(x = x,
              weight = weight,
              mixed_aggregation = NULL,
              unit = units,
              description = "Producer prices in 05USDppp/tDM"))
}
