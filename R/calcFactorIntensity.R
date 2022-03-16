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


  if (method == "USDA") { # using USDA mehod

    # Production of crops. mio. ton
      crop_prod_dm_All  <- collapseDim(calcOutput("Production", products = "kcr", aggregate = FALSE, attributes = "dm"))

      VoPcrops <- calcOutput("VoPcrops", output = "absolute", aggregate = FALSE) # mio. 05USD MER
      getNames(VoPcrops)[getNames(VoPcrops) == "oilpalm_fruit"] <- "oilpalm"

      gnames <- intersect(getNames(VoPcrops), getNames(crop_prod_dm_All))
      gyears <- intersect(getYears(VoPcrops), getYears(crop_prod_dm_All))
      gcells <- intersect(getCells(VoPcrops), getCells(crop_prod_dm_All))

      # Value of production per ton produced

      VoP_perTon <- VoPcrops[gcells, gyears, gnames] / crop_prod_dm_All[gcells, gyears, gnames]
      VoP_perTon[!is.finite(VoP_perTon)] <- 0

      # Fraction of each capital and labour input in overall value of production
      fraction_inputs <- calcOutput("FractionInputsUSDA", aggregate = FALSE)[, , c("Capital", "Labor")]

      fyears <- intersect(getYears(fraction_inputs), getYears(VoP_perTon))


      # Calculation of capital and labor intensities, and Capital share between the two
      Intensity <- fraction_inputs[, fyears, ] * VoP_perTon[, fyears, ]
      Share_Capital <- fraction_inputs[, fyears, c("Capital")] / dimSums(fraction_inputs[, fyears, ], dim = 3.1)



       # assuming a 4% interest rate and 5% depreciation
       x <- if (output == "intensities") Intensity else if (output == "requirements") Intensity[, , c("Capital", "Labor")] /
         (0.04 + 0.05) else if (output == "CapitalShare") Share_Capital else stop("Output not supported")
       x["PHL", , ] <- 0 # inconsistent data in Philippines
       x[!is.finite(x)] <- 0
       weight <- x

       if (output != "CapitalShare") {
         weight[, , "Capital"] <- crop_prod_dm_All[, fyears, gnames]
         weight[, , "Labor"] <- crop_prod_dm_All[, fyears, gnames]
         weight[!is.finite(x)] <- 0
         weight[x == 0] <- 0
       } else {
         weight[, , "Capital"] <- dimSums(crop_prod_dm_All[, fyears, ], dim = 3)
         weight[!is.finite(x)] <- 0
         weight[x == 0] <- 0

       }


   } else if (method == "CapitalStock" & output %in% c("intensities", "requirements")) {

          # Fraction of each crop on overall Value of Production (Agriculture, Forestry and Fisheries)
          fraction_VoP_crop <- calcOutput("VoPcrops", output = "fraction", aggregate = FALSE)

          # Existing capital stocks
          name <- "22034|Net Capital Stocks (Agriculture, Forestry and Fishing).Value_USD_2015_prices_(millions)"
          CapitalStocks_currentUSD <- readSource("FAO_online", "CapitalStock", convert = TRUE)[, , name]
          CapitalStocks <- convertGDP(CapitalStocks_currentUSD, unit_in = "current US$MER", unit_out = "constant 2005 US$MER")
          # for countries with missing conversion factors we assume no inflation:
          CapitalStocks[is.na(CapitalStocks)] <- CapitalStocks_currentUSD[is.na(CapitalStocks)]

          years <- intersect(getYears(fraction_VoP_crop), getYears(CapitalStocks))
          region <- intersect(getCells(fraction_VoP_crop), getCells(CapitalStocks))

          # Capital stocks per crop
          CapitalStocks_crop <- collapseDim(CapitalStocks[region, years, ] *
                                              fraction_VoP_crop[region, years, ])

          # Production
          Production <- collapseDim(calcOutput("Production", products = "kcr", aggregate = FALSE)[, , "dm"])


          if (output %in% c("requirements", "intensities")) {

              names <- intersect(getNames(Production), getNames(CapitalStocks_crop))
              years <- intersect(getYears(Production), getYears(CapitalStocks_crop))
              cells <- intersect(getCells(Production), getCells(CapitalStocks_crop))

              # Capital stock requirements
              x <- CapitalStocks_crop[cells, years, names] / Production[cells, years, names]
              x[!is.finite(x)] <- 0

          } else {

            stop("Output not supported")

          }

          # assuming a 4% interest rate and a 5% depreciation rate

          x <- if (output == "intensities") x * (0.04 + 0.05) else if (output == "requirements") x
          x["PHL", , ] <- 0

          weight <- x
          weight[, , ] <- Production[, getYears(x), getNames(x)]
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
