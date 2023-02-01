#' @title calcFactorIntensity
#' @description Calculates factor intensity for labour and/or capital from USDA (Inputs share)
#' and FAO (Value of Production)in 05USDppp per ton.
#' Capital intensity and requirements can also be calculated from FAO's CapitalStock database.
#'
#'
#'
#' @param output needed outputs. It can be either "intensities" (Capital/Labour factor intensities),
#' "requirements" (Capital Stock requirements per ton), and "CapitalShare" for "USDA" method.
#' For the "CapitalStock" method only "intensities" and "requirements" outputs supported.
#' @param method "USDA" or "CapitalStock"
#' @return magpie object of the factor requirements intensity or factor intensity in 05USDppp/tDM per crop,
#' or capital share fraction.
#' @author Edna J. Molina Bacca
#' @importFrom luscale speed_aggregate
#' @importFrom dplyr  intersect
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("FactorIntensity")
#' }
#'
calcFactorIntensity <- function(output = "intensities", method = "USDA") {


  if (method == "USDA") { # using USDA method

    # Production of crops. mio. ton
      cropProdDMall  <- collapseDim(calcOutput("Production", products = "kcr", aggregate = FALSE, attributes = "dm"))

      vopCrops <- calcOutput("VoPcrops", aggregate = FALSE) # mio. 05USD MER

      gnames <- intersect(getNames(vopCrops), getNames(cropProdDMall))
      gyears <- intersect(getYears(vopCrops), getYears(cropProdDMall))
      gcells <- intersect(getCells(vopCrops), getCells(cropProdDMall))

      # Value of production per ton produced
      vopPerTon <- vopCrops[gcells, gyears, gnames] / cropProdDMall[gcells, gyears, gnames]
      vopPerTon[!is.finite(vopPerTon)] <- 0

      # Fraction of each capital and labour input in overall value of production
      fractionInputs <- calcOutput("FractionInputsUSDA", aggregate = FALSE)[, , c("Capital", "Labor")]

      fyears <- intersect(getYears(fractionInputs), getYears(vopPerTon))

      # Calculation of capital and labor intensities, and Capital share between the two
      intensity <- fractionInputs[, fyears, ] * vopPerTon[, fyears, ]
      shareCapital <- fractionInputs[, fyears, c("Capital")] / dimSums(fractionInputs[, fyears, ], dim = 3.1)

       # assuming a 4% interest rate and 5% depreciation
       x <- if (output == "intensities") intensity else if (output == "requirements") intensity[, , c("Capital",
          "Labor")] / (0.04 + 0.05) else if (output == "CapitalShare") shareCapital else stop("Output not supported")
       x["PHL", , ] <- 0 # inconsistent data in Philippines
       x[!is.finite(x)] <- 0
       weight <- x

       if (output != "CapitalShare") {
         weight[, , "Capital"] <- cropProdDMall[, fyears, gnames]
         weight[, , "Labor"] <- cropProdDMall[, fyears, gnames]
         weight[!is.finite(x)] <- 0
         weight[x == 0] <- 0
       } else {
         weight[, , "Capital"] <- dimSums(cropProdDMall[, fyears, ], dim = 3)
         weight[!is.finite(x)] <- 0
         weight[x == 0] <- 0

       }


   } else if (method == "CapitalStock" && output %in% c("intensities", "requirements")) {

          # Fraction of each crop on overall Value of Production (Agriculture, Forestry and Fisheries)
          vopCrops <- calcOutput("VoPcrops", aggregate = FALSE)
          vopAff <- dimSums(calcOutput("VoPAFF", aggregate = FALSE), dim = 3)
          years <- intersect(getYears(vopCrops), getYears(vopAff))
          fractionVoPcrop <- vopCrops[, years, ] / vopAff[, years, ]
          fractionVoPcrop[!is.finite(fractionVoPcrop)] <- 0

          # Existing capital stocks
          name <- "22034|Net Capital Stocks (Agriculture, Forestry and Fishing).Value_USD_2015_prices_(millions)"
          capitalStocks <- readSource("FAO_online", "CapitalStock", convert = TRUE)[, , name]
          capitalStocks <- convertGDP(capitalStocks, unit_in = "current US$MER",
                                                     unit_out = "constant 2005 US$MER",
                                                     replace_NAs = "no_conversion")

          years <- intersect(getYears(fractionVoPcrop), getYears(capitalStocks))
          region <- intersect(getCells(fractionVoPcrop), getCells(capitalStocks))

          # Capital stocks per crop
          capitalStocksCrop <- collapseDim(capitalStocks[region, years, ] *
                                              fractionVoPcrop[region, years, ])

          # Production
          production <- collapseDim(calcOutput("Production", products = "kcr", aggregate = FALSE)[, , "dm"])


          if (output %in% c("requirements", "intensities")) {

              names <- intersect(getNames(production), getNames(capitalStocksCrop))
              years <- intersect(getYears(production), getYears(capitalStocksCrop))
              cells <- intersect(getCells(production), getCells(capitalStocksCrop))

              # Capital stock requirements
              x <- capitalStocksCrop[cells, years, names] / production[cells, years, names]
              x[!is.finite(x)] <- 0

          } else {

            stop("Output not supported")

          }

          # assuming a 4% interest rate and a 5% depreciation rate

          x <- if (output == "intensities") x * (0.04 + 0.05) else if (output == "requirements") x
          x["PHL", , ] <- 0

          weight <- x
          weight[, , ] <- production[, getYears(x), getNames(x)]
          weight[!is.finite(x)] <- 0
          weight[x == 0] <- 0

  } else {
    stop("Method or output not supported")
  }

   units <-
   if (output %in% c("intensities", "requirements")) "05USDMER/tDM" else if (output == "CapitalShare") "fraction"


   return(list(x = x,
               weight = weight,
               mixed_aggregation = NULL,
               unit = units,
               description = "Factor Intensities or capital requirements for different
                            crops in USD05 per ton or fraction"))
}
