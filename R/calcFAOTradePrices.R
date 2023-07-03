#' @title calcFAOTradePrices
#'
#' @description calculates USD per kg of FAOSTAT Trade data
#'              for import and export prices
#'
#' @param aggregation "none", "k", "fbs" or "springmann"
#'                    for the last uses Marco Springmann's custom product mapping
#'
#' @return List of magpie objects with results on country level,
#'         weight on country level, unit and description.
#' @author David M Chen
#' @examples
#' \dontrun{
#' calcOutput("calcFAOTradePrices")
#' }
#'
calcFAOTradePrices <- function(aggregation = "k") {

  trade <- readSource("FAO_online", subtype = "Trade")

  # no conversion for heads or numbers of animals yet?
  trade <- trade[, , c("export", "import", "import_US$MER05", "export_US$MER05")]

  # get mapping
  mapping <- toolGetMapping("FAOitems_online.csv", type = "sectoral", where = "mappingfolder")

  if (aggregation == "k") {
    tradeAgg <- toolAggregate(trade, rel = mapping,
                               from = "ProductionItem", to = "k",
                               partrel = TRUE, dim = 3.1)
  } else if (aggregation == "fbs") {
    tradeAgg <- toolAggregate(trade, rel = mapping,
                               from = "ProductionItem", to = "FoodBalanceItem",
                               partrel = TRUE, dim = 3.1)
  } else if (aggregation == "springmann") {
    tradeFBS <- toolAggregate(trade, rel = mapping,
                               from = "ProductionItem", to = "FoodBalanceItem",
                               partrel = TRUE, dim = 3.1)
    mappingSpringmann <- toolGetMapping("springmann_fbs_mapping.csv",
                                        type = "sectoral", where = "mappingfolder")
    getNames(tradeFBS, dim = 1) <- gsub(".*\\|", "", getNames(tradeFBS, dim = 1))
    tradeAgg <- toolAggregate(tradeFBS, rel = mappingSpringmann,
                               from = "FBS.item", to = "Food.group",
                              partrel = TRUE, dim = 3.1)
  } else if (aggregation == "none") {
    tradeAgg <- trade
  } else {
    stop("Only none k fbs and springmann aggregations currently")
  }

  importPrice <- collapseNames(tradeAgg[, , "import_kUS$"] / tradeAgg[, , "import"],
                               collapsedim = 3)
  getNames(importPrice, dim = 2) <- "importPrice"

  exportPrice <- collapseNames(tradeAgg[, , "export_kUS$"] / tradeAgg[, , "export"],
                               collapsedim = 3)
  getNames(exportPriceS, dim = 2) <- "exportPrice"

  out <- mbind(importPrice, exportPrice)

  weight <- tradeAgg[, , c("export", "import")]
  getNames(weight, dim = 2) <- c("importPrice", "exportPrice")

  return(list(x = out,
              weight = weight,
              unit = "$/t",
              description = "FAO Prices at Trade"))

}
