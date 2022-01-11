#' @title calcVoP_AFF
#' @description Calculates the overall value of production of the agriculture,
#' forestry and fisheries sectors. Forestry and Fisheries are calculated from exports values.
#'
#' @return magpie object. in mio. 05USD MER units
#' @author Edna J. Molina Bacca, Debbora Leip
#' @importFrom dplyr intersect
#' @importFrom magclass dimSums
#' @importFrom GDPuc convertGDP
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("VoP_AFF")
#' }
#'
calcVoP_AFF <- function() {

#### Value of production for Agriculture (crops and livestock)
  agItems <- c("2041|Crops", "2044|Livestock")
  unit <- "Gross_Production_Value_(current_thousand_US$)_(1000_US$)"
  
  # conversion from current USD MER to constant 2005 USD MER
  VoP_ag_curMER <- dimSums(readSource("FAO_online", "ValueOfProd")[, , list(agItems, unit)] / 1000, dim = 3) # mio. US$
  VoP_ag <- convertGDP(VoP_ag_curMER,
                       unit_in = "current US$MER",
                       unit_out = "constant 2005 US$MER")
  # for countries with missing conversion factor we assume no inflation:
  VoP_ag[is.na(VoP_ag)] <- VoP_ag_curMER[is.na(VoP_ag)]
  getNames(VoP_ag) <- "Agriculture"

#### Value of production fisheries
  
  # export value and quantity of fish and other aquatic products
  export_fish_value <- readSource("FishstatJ_FAO", subtype = "exportsValue") # 1000 current USD MER
  export_fish_tonNet <- readSource("FishstatJ_FAO", subtype = "exportsQuantity") # ton_net

  Fish_Cat <- c("2961|Aquatic Products, Other + (Total).production",
              "2960|Fish, Seafood + (Total).production")

  production_fish_tonNet <- dimSums(readSource("FAO", "CBLive")[, , Fish_Cat], dim = 3.1) # ton net


  # common years
  years_fish <- intersect(intersect(getYears(export_fish_value),
                                    getYears((export_fish_tonNet))), getYears(production_fish_tonNet))
  cells_fish <- intersect(intersect(getCells(export_fish_value),
                                    getCells((export_fish_tonNet))), getCells(production_fish_tonNet))


  # Value of production for fish and aquatic products -> Production*export_price
  VoP_fish_currentUSD <- export_fish_value[cells_fish, years_fish, ] / export_fish_tonNet[cells_fish, years_fish, ] *
             production_fish_tonNet[cells_fish, years_fish, ] / 1000  # mio. current USD
  VoP_fish <- convertGDP(VoP_fish_currentUSD,
                         unit_in = "current US$MER",
                         unit_out = "constant 2005 US$MER")
  # for countries with missing inflation factors we assume no inflation
  VoP_fish[is.na(VoP_fish)] <- VoP_fish_currentUSD[is.na(VoP_fish)]

  VoP_fish[!is.finite(VoP_fish)] <- 0
  getNames(VoP_fish) <- "Fisheries"


#### Value of production forestry

  Forest_cat <- c("Roundwood.Export_Value_(Mio_US$)",
                "Roundwood.Export_Quantity_(m3)",
                "Roundwood.Production_(m3)")

  VoP_forestry_data <- readSource("FAO", "ForestProdTrade")[, , Forest_cat]

  price_forestry_currentUSD <- VoP_forestry_data[, , "Roundwood.Export_Value_(Mio_US$)"] /
                VoP_forestry_data[, , "Roundwood.Export_Quantity_(m3)"]

  # Base year change for exports value
  price_forestry <- convertGDP(price_forestry_currentUSD,
                               unit_in = "current US$MER",
                               unit_out = "constant 2005 US$MER")
  # for countries with missing inflation factors we assume no inflation
  price_forestry[is.na(price_forestry)] <- price_forestry_currentUSD[is.na(price_forestry)]

  price_forestry[!is.finite(price_forestry)] <- 0


  years <- intersect(getYears(price_forestry), getYears(VoP_forestry_data))

  VoP_forestry <- toolCountryFill(x = VoP_forestry_data[, years, "Roundwood.Production_(m3)"] *
                                    price_forestry[, years, ], fill = 0) # mio. constant 2005 US$MER
  getNames(VoP_forestry) <- "Forestry"

################

  # magpie object to return
  years_VoP <- intersect(intersect(getYears(VoP_ag), getYears((VoP_fish))), getYears(VoP_forestry))
  cells_VoP <- intersect(intersect(getCells(VoP_ag), getCells((VoP_fish))), getCells(VoP_forestry))

  x <- mbind(VoP_ag[cells_VoP, years_VoP, ], VoP_fish[cells_VoP, years_VoP, ],
             VoP_forestry[cells_VoP, years_VoP, ])
  x[!is.finite(x)] <- 0


  return(list(x = x,
         weight = NULL,
         mixed_aggregation = NULL,
         unit = "mio. 05USDmer units",
         description = " Value of production for the agriculture, forestry and fisheries sector"))
}
