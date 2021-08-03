#' @title calcVoP_AFF
#' @description Calculates the overall value of production of the agriculture,
#' forestry and fisheries sectors. Forestry and Fisheries are calculated from exports values.
#'
#'
#'
#' @return magpie object. in mio. 05USD ppp units
#' @author Edna J. Molina Bacca
#' @importFrom dplyr intersect
#' @importFrom magclass dimSums
#' @seealso \code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' a <- calcOutput("VoP_AFF")
#' }
#'
calcVoP_AFF <- function() {

#### GDP
  GDP_ppp <- calcOutput("GDPppp", aggregate = FALSE, FiveYearSteps = FALSE)[, , "gdp_SSP2"]
  GDP_mer <- readSource("WDI", "NY.GDP.MKTP.CD")
  GDP_con <- setNames(setYears((GDP_ppp[, 2005, ] / GDP_ppp[, 2015, ]), NULL), NULL)

#### Value of production for Agriculture (crops and livestock)
  Ag <- c("2041|Crops.Gross_Production_Value_(constant_2014_2016_thousand_I$)_(1000_Int_$)",
        "2044|Livestock.Gross_Production_Value_(constant_2014_2016_thousand_I$)_(1000_Int_$)")

   # factor converts from mio. 2015 USD to 2005 USD
  VoP_agriculture <- dimSums(readSource("FAO_online", "ValueOfProd")[, , Ag] / 1000, dim = 3) * GDP_con
  getNames(VoP_agriculture) <- "Agriculture"


#### Value of production fisheries


  # export value and quantity of fish and other aquatic products
  export_fish_value <- readSource("FishstatJ_FAO", subtype = "exportsValue") # 1000 current USD
  export_fish_tonNet <- readSource("FishstatJ_FAO", subtype = "exportsQuantity") # ton_net

  Fish_Cat <- c("2961|Aquatic Products, Other + (Total).production",
              "2960|Fish, Seafood + (Total).production")

  production_fish_tonNet <- dimSums(readSource("FAO", "CBLive")[, , Fish_Cat], dim = 3.1) # ton net


  # common years
  years_fish <- intersect(intersect(getYears(export_fish_value),
                                    getYears((export_fish_tonNet))), getYears(production_fish_tonNet))
  cells_fish <- intersect(intersect(getCells(export_fish_value),
                                    getCells((export_fish_tonNet))), getCells(production_fish_tonNet))

  # Base year and USD type change
  GDP_con_mer <- setYears(GDP_mer[, 2005, ] / GDP_mer[, years_fish, ], years_fish)
  GDP_con <- GDP_con_mer * setYears(GDP_ppp[, 2005, ] / GDP_mer[, 2005, ], NULL)

  # Value of production for fish and aquatic products -> Production*export_price
  VoP_fish <- export_fish_value[cells_fish, years_fish, ] / export_fish_tonNet[cells_fish, years_fish, ] *
               production_fish_tonNet[cells_fish, years_fish, ] / 1000 * GDP_con # mio. 2005 USD
  VoP_fish[!is.finite(VoP_fish)] <- 0
  getNames(VoP_fish) <- "Fisheries"


#### Value of production forestry

  Forest_cat <- c("Roundwood.Export_Value_(Mio_US$)",
                "Roundwood.Export_Quantity_(m3)",
                "Roundwood.Production_(m3)")

  VoP_forestry_data <- readSource("FAO", "ForestProdTrade")[, , Forest_cat]

  price_forestry <- VoP_forestry_data[, , "Roundwood.Export_Value_(Mio_US$)"] / 
                VoP_forestry_data[, , "Roundwood.Export_Quantity_(m3)"]


  # Base year change for exports value
  years <- getYears(price_forestry)
  
  GDP_con_mer <- setYears(GDP_mer[, 2005, ] / GDP_mer[, years, ], years)
  GDP_con <- GDP_con_mer * setYears(GDP_ppp[, 2005, ] / GDP_mer[, 2005, ], NULL)
  
  price_forestry <- price_forestry[, years, ] * GDP_con
  price_forestry[!is.finite(price_forestry)] <- 0

  years <- intersect(getYears(price_forestry), getYears(VoP_forestry_data))
  # mio. current USD
  VoP_forestry <- toolCountryFill(x = VoP_forestry_data[, years, "Roundwood.Production_(m3)"] * 
                                    price_forestry[, years, ], fill = 0)
  getNames(VoP_forestry) <- "Forestry"

################

  # magpie object to return
  years_VoP <- intersect(intersect(getYears(VoP_agriculture), getYears((VoP_fish))), getYears(VoP_forestry))
  cells_VoP <- intersect(intersect(getCells(VoP_agriculture), getCells((VoP_fish))), getCells(VoP_forestry))

  x <- mbind(VoP_agriculture[cells_VoP, years_VoP, ], VoP_fish[cells_VoP, years_VoP, ],
             VoP_forestry[cells_VoP, years_VoP, ])
  x[!is.finite(x)] <- 0


  return(list(x = x,
         weight = NULL,
         mixed_aggregation = NULL,
         unit = "mio. current USD units",
         description = " Value of production for the agriculture, forestry and fisheries sector"))
}
