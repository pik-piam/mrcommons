#' @title calcPriceAgriculture
#'
#' @description provides global prices from the IMPACT model projections,
#'              World Bank observations, and FAO observations
#'              for MAgPIE commodities in $/tDM
#'
#' @param datasource Options of the source of data:
#'                   `IMPACT3.2.2World_Price`, `FAO`, `FAOp` and `WBGEM`
#' @param unit       A string with the unit that should be returned.
#'                   Options are:
#'   \itemize{
#'     \item "current LCU"
#'     \item "current Int$PPP"
#'     \item "current US$MER"
#'     \item "constant YYYY LCU"
#'     \item "constant YYYY Int$PPP"
#'     \item "constant YYYY US$MER"
#'   }
#'
#' @return List with a magpie object with commodity prices
#' @author Mishko Stevanovic, Xiaoxi Wang, Felicitas Beier
#' @seealso
#' [readIMPACT3.2.2World_Price()],
#' [calcWBGEM()],
#' [readWBGEM()]
#' @examples
#' \dontrun{
#' calcOutput("PriceAgriculture", datasource = "IMPACT3.2.2World_Price", aggregate = FALSE)
#' calcOutput("PriceAgriculture", datasource = "FAO")
#' calcOutput("PriceAgriculture", datasource = "WBGEM", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset reporthelper
#' @importFrom magclass wrap
#' @importFrom dplyr %>% group_by summarise n
#' @importFrom rlang .data
#' @importFrom reshape2 melt acast
#'

calcPriceAgriculture <- function(datasource = "IMPACT3.2.2World_Price", unit = "US$17/tDM") {

  if (datasource == "IMPACT3.2.2World_Price") {

    out <- readSource(type = "IMPACT3.2.2World_Price")

    # get the mapping
    mapping <- toolGetMapping("impact2magpie.csv", type = "sectoral", where = "mappingfolder")
    mapping <- mapping[-which(mapping$MAgPIE == ""), ]

    # aggregate to MAgPIE crops
    out <- toolAggregate(out, rel = mapping, from = "IMPACT", to = "MAgPIE",
                         dim = 3.2, partrel = TRUE, verbosity = 2)

    # correct the prices for dry matter values
    dm <- 1 / readSource("ProductAttributes", "Products")[, , "wm"]
    dm <- collapseNames(dm)
    commodities <- unique(mapping$MAgPIE)
    ktradeSet <- findset("k_trade")
    out <- out[, , ] / dm[, , commodities]

    out <- add_dimension(out, dim = 3.2, add = "model", nm = "IMPACT")
    names(dimnames(out))[3] <- "scenario.model.variable"

    description  <- paste0("Prices from the IMPACT model projections. There are ",
                           length(ktradeSet) - length(commodities),
                           " missing MAgPIE commodities: ",
                           paste(ktradeSet[!ktradeSet %in% commodities], collapse = " "))
    weight       <- NULL
    isocountries <- FALSE

  } else if (datasource == "WBGEM") {

    # Prices in US$17/tDM
    x <- calcOutput("WBGEM", aggregate = FALSE)

    # sectoral mappings
    mapping <- toolGetMapping(type = "sectoral", name = "mappingWBGEM2MAgPIEnew.csv", where = "mappingfolder")
    mapping <- mapping[mapping$wbgem %in% getNames(x), ]

    x   <- x[, , mapping$wbgem]
    tmp <- mapping %>% group_by(.data$magpie) %>% summarise(V1 = n())

    tmp    <- base::merge(mapping, tmp, by = "magpie")
    weight <- as.magpie(acast(melt(tmp), . ~ wbgem))
    getNames(weight) <- gsub("\\..", "", getNames(weight))

    x <- toolAggregate(x, mapping, from = "wbgem", to = "magpie", weight = weight, dim = 3)

    dm <- 1 / readSource("ProductAttributes", "Products")[, , "wm"]
    dm <- collapseNames(dm)
    ktradeSet <- findset("k_trade")

    out <- NULL
    for (i in getNames(x, dim = 1)) {
      out <- mbind(out, x[, , i] / setNames(dm[, , i], NULL))
    }
    rm(x)

    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)

    names(dimnames(out))[3] <- gsub("data1", "variable", names(dimnames(out))[3])

    description <- paste0("Average prices from the WBGEM-Commodities. There are ",
                          length(ktradeSet) - length(getNames(out)), " missing MAgPIE commodities: ",
                          paste(ktradeSet[!ktradeSet %in% getNames(out)], collapse = " "))
    weight <- NULL
    isocountries <- FALSE

  } else if (datasource == "FAOp") {

    # calculate prices as a ratio of production value and production quantities
    vprod <- calcOutput("AgProductionValue", aggregate = FALSE, datasource = "FAO")
    qprod <- calcOutput("FAOharmonized", aggregate = FALSE)
    aggregation <- toolGetMapping("FAOitems.csv",
                                  type = "sectoral", where = "mappingfolder")

    # aggregate FAO quantities into MAgPIE commodities
    qprod <- toolAggregate(qprod[, , "production"], rel = aggregation,
                           from = "FoodBalanceItem", to = "k",
                           dim = 3.1, partrel = TRUE, verbosity = 2)
    qprod <- collapseNames(qprod)

    ## convert to DM tonnes
    dm <- 1 / readSource("ProductAttributes", "Products")[, , "wm"]
    dm <- collapseNames(dm)
    comms <- intersect(intersect(getNames(dm), getNames(qprod)), getNames(vprod, dim = 3))
    qprod <- qprod[, , comms] * dm[, , comms]

    years  <- intersect(getYears(qprod), getYears(vprod))
    out    <- vprod[, years, comms] / qprod[, years, comms]
    weight <- qprod
    weight[is.na(out) | is.infinite(out)] <- 0
    out[is.na(out) | is.infinite(out)]    <- 0

    description <- "FAO prices Agricultural Value statistics."
    isocountries <- TRUE

  } else if (datasource == "FAO") {

    # Annual producer prices in US$17/tDM
    out         <- readSource("FAO_online", subtype = "PricesProducerAnnual", convert = TRUE)
    aggregation <- toolGetMapping("FAOitems_online_2010update.csv",
                                  type = "sectoral", where = "mrfaocore")

    qprod <- collapseNames(calcOutput("FAOharmonized", src = "join2010", aggregate = FALSE)[, , "production"])

    # "2577|Palm Oil", "2576|Palmkernel Oil", "2595|Palmkernel Cake" need to be aggregated to get "oilpalm"
    # remove the original "oilpalm" which has only area harvested
    qprod[, , "oilpalm"] <- dimSums(qprod[, , c("2577|Palm Oil", "2576|Palmkernel Oil", "259|Cake of palm kernel")])

    qprod <- toolAggregate(qprod, rel = aggregation, from = "post2010_FoodBalanceItem",
                           to = "post2010_ProductionItem", dim = 3, partrel = TRUE, verbosity = 2)

    comms <- intersect(getNames(out), getNames(qprod))
    years <- intersect(getYears(out), getYears(qprod))
    qprod <- qprod[, years, comms]
    out   <- out[, years, comms]

    # setting weight for missing price data to zero
    qprod[out == 0] <- 0

    # weighted aggregation of fao prices to magpie commodities
    out <- toolAggregate(out, rel = aggregation, weight = qprod + 10^-10, from = "post2010_ProductionItem",
                         to = "k", dim = 3, partrel = TRUE, verbosity = 2)
    out <- out[, , -which(getNames(out) %in% c("remaining", "not_clear", ""), arr.ind = TRUE)]

    # correct the prices for dry matter values
    dm <- 1 / readSource("ProductAttributes", "Products")[, , "wm"]
    dm <- collapseNames(dm)
    out <- out / dm[, , getNames(out)]
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)
    names(dimnames(out))[3] <- gsub("data1", "variable", names(dimnames(out))[3])

    weight <- toolAggregate(qprod, rel = aggregation, from = "post2010_ProductionItem",
                            to = "k", dim = 3.1, partrel = TRUE, verbosity = 2)
    weight <- weight[, , -which(getNames(weight) %in% c("remaining", "not_clear", ""), arr.ind = TRUE)]
    description <- "FAO prices based on Annual Produces Prices statistics."
    isocountries <- TRUE
  }

  if (unit != "US$17/tDM") {
    # Transform to selected currency unit
    out <- GDPuc::toolConvertGDP(out,
                                 unit_in = "constant 2017 US$MER",
                                 unit_out =  unit,
                                 replace_NAs = "no_conversion")
  }

  if (!is.null(weight)) {
    weight <- weight + 10^-10
  }

  return(list(x = out,
              weight = weight,
              unit = unit,
              description = description,
              isocountries = isocountries))
}
