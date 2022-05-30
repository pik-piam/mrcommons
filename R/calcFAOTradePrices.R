#' @title calcFAOTradePrices
#' @description calculates USD per kg of FAOSTAT Trade data for import and export prices
#' @param aggregation "none", "k", "fbs" or "springmann" the last uses Marco Springmann's custom product mapping
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author David M Chen
#' @examples
#' \dontrun{
#' calcOutput("calcFAOTradePrices")
#' }

calcFAOTradePrices <- function(aggregation = "k") {

  trade <- readSource("FAO_online", subtype = "Trade")

  #no conversion for heads or numbers of animals yet?
  trade <- trade[,,c("export", "import", "export_kUS$", "import_kUS$")]

  #get mapping
  mapping <- toolGetMapping("FAOitems_online.csv", type = "sectoral")

  if (aggregation == "k"){
    trade_agg <- toolAggregate(trade, rel = mapping, from = "ProductionItem", to = "k", partrel = TRUE, dim = 3.1)
  } else if (aggregation == "fbs") {
    trade_agg <- toolAggregate(trade, rel = mapping, from = "ProductionItem", to = "FoodBalanceItem", partrel = TRUE, dim = 3.1)
  } else if (aggregation == "springmann") {
    trade_fbs <- toolAggregate(trade, rel = mapping, from = "ProductionItem", to = "FoodBalanceItem", partrel = TRUE, dim = 3.1)
    springmann_mapping <- toolGetMapping("springmann_fbs_mapping.csv", type = "sectoral")
    getNames(trade_fbs, dim = 1) <- gsub(".*\\|", "", getNames(trade_fbs, dim = 1))
    trade_agg <- toolAggregate(trade_fbs, rel = springmann_mapping,
                               from = "FBS.item", to = "Food.group", partrel = TRUE, dim = 3.1)
  } else if (aggregation == "none") {
    trade_agg <- trade
  } else stop("Only none k fbs and springmann aggregations currently")

  import_prices <- collapseNames(trade_agg[,,"import_kUS$"] / trade_agg[,,"import"], collapsedim = 3)
  getNames(import_prices, dim = 2) <- "importPrice_$kg"

  export_prices <- collapseNames(trade_agg[,,"export_kUS$"] / trade_agg[,,"export"], collapsedim = 3)
  getNames(export_prices, dim = 2) <- "exportPrice_$kg"

  out <- mbind(import_prices, export_prices)

  weight <- trade_agg[,,c("export", "import")]
  getNames(weight, dim = 2) <- c("importPrice_$kg", "exportPrice_$kg")

  return(list(x = out,
              weight = weight,
              unit = "$/kg",
              description = "Employment in agriculture, forestry and fishery (based on ILO modelled estimates)"))

}
