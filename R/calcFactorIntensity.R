#' @title calcFactorIntensity
#' @description Calculates factor intensity for labour and/or capital from USDA (Inputs share)
#' and FAO (Value of Production)in USD05 per ton.
#' Capital intensity and requirements can also be calculated from FAO's CapitalStock database.
#'
#'
#'
#' @param output needed outputs. It can be either "intensities" (Capital/Labour factor intensities),
#' "requirements" (Capital Stock requirements per ton), and "CapitalShare" for "USDA" method.
#' For the "CapitalStock" method only "intensities" and "requirements" outputs supported.
#' @param method "USDA" or "CapitalStock"
#' @return magpie object of the factor requirements intensity or factor intensity in USD05/ton per crop, 
#' or capital share fraction.
#' @author Edna J. Molina Bacca
#' @importFrom luscale speed_aggregate
#' @importFrom dplyr  intersect
#'
#' @seealso \code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' a <- calcOutput("FactorIntensity")
#' }
#'
calcFactorIntensity <- function(output = "intensities", method = "USDA") {


  if (method == "USDA") { # using USDA mehod

    # Production of crops. mio. ton
      crop_prod_dm_All  <- collapseDim(calcOutput("Production", products = "kcr", aggregate = FALSE, attributes = "dm"))

      VoP_crops <- calcOutput("VoP_crops", output = "absolute", aggregate = FALSE) # mio. USD05
      getNames(VoP_crops)[getNames(VoP_crops) == "oilpalm_fruit"] <- "oilpalm"

      gnames <- intersect(getNames(VoP_crops), getNames(crop_prod_dm_All))
      gyears <- intersect(getYears(VoP_crops), getYears(crop_prod_dm_All))
      gcells <- intersect(getCells(VoP_crops), getCells(crop_prod_dm_All))

      # Value of production per ton produced

      VoP_perTon <- VoP_crops[gcells, gyears, gnames] / crop_prod_dm_All[gcells, gyears, gnames]
      VoP_perTon[!is.finite(VoP_perTon)] <- 0

      # Fraction of each capital and labour input in overall value of production
      fraction_inputs <- calcOutput("FractionInputsUSDA", aggregate = FALSE)[, , c("Capital", "Labor")]

      fyears <- intersect(getYears(fraction_inputs), getYears(VoP_perTon))


      # Calculation of capital and labor intensities, and Capital share between the two
      Intensity <- fraction_inputs[, fyears, ] * VoP_perTon[, fyears, ]
      Share_Capital <- fraction_inputs[, , "Capital"] / dimSums(fraction_inputs, dim = 3.1)



       # assuming a 4% interest rate and 5% depreciation
       x <- if (output == "intensities") Intensity else if (output == "requirements") Intensity[, , "Capital"] /
         (0.04 + 0.05) else if (output == "CapitalShare") Share_Capital else stop("Output not supported")
       x["PHL", , ] <- 0 # inconsistent data in Philippines

       weight <- x
       weight[, , "Capital"] <- crop_prod_dm_All[, fyears, gnames]
       weight[, , "Labor"] <- crop_prod_dm_All[, fyears, gnames]
       weight[!is.finite(x)] <- 0
       weight[x == 0] <- 0


   } else if (method == "CapitalStock" & output %in% c("intensities", "requirements")) {

          # GDP for units conversio
          GDP <- calcOutput("GDPppp", aggregate = FALSE, FiveYearSteps = FALSE)[, , "gdp_SSP2"]
          GDP_con <- setNames(setYears((GDP[, 2005, ] / GDP[, 2015, ]), NULL), NULL)

          # Fraction of each crop on overall Value of Production (Agriculture, Forestry and Fisheries)
          fraction_VoP_crop <- calcOutput("VoP_crops", output = "fraction", aggregate = FALSE)

          # Existing capital stocks
          name <- "22034|Net Capital Stocks (Agriculture, Forestry and Fishing).Value_USD_2015_prices_(millions)"
          CapitalStocks <- readSource("FAO_online", "CapitalStock", convert = TRUE)[, , name]
          years <- intersect(getYears(fraction_VoP_crop), getYears(CapitalStocks))
          region <- intersect(getCells(fraction_VoP_crop), getCells(CapitalStocks))

          # Capital stocks per crop
          CapitalStocks_crop <- collapseDim(CapitalStocks[region, years, ] * 
                                              fraction_VoP_crop[region, years, ]) * GDP_con 

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

   units <- if (output %in% c("intensities", "requirements")) "USD05/ton" else if (output == "CapitalShare") "fraction"



   return(list(x = x,
               weight = weight,
               mixed_aggregation = NULL,
               unit = units,
               description = "Factor Intensities or capital requirements for different crops in USD05 per ton or fraction"))
}
