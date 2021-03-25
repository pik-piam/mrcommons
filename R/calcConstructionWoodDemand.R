#' @title calcConstructionWoodDemand
#' @description
#' Calculates the demand of construction wood from Galina et al. 2020 data.
#' See
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{calcFAOmassbalance_pre}}
#' @examples
#'
#' \dontrun{
#' calcOutput("ConstructionWoodDemand")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @export

calcConstructionWoodDemand <- function() {

  ## Read SSP data
  ssp_data <- readSource(type = "SSP", subtype = "all")[, , c("Population", "Population|Urban|Share")]

  ## All years multiples of 5 (not 10) have no values coming in from SSP database. Also 2000 has no values and 1995 is missing
  ## Use linear interpolation

  ## Generate missing years
  missing_years <- c("y1995", "y2000", paste0("y", seq(2005, 2095, 10)))

  ## Remove missing years
  ssp_data <- ssp_data[, missing_years, , invert = TRUE]

  ## Add back missing years
  ssp_data <- time_interpolate(dataset = ssp_data, interpolated_year = missing_years, integrate_interpolated_years = TRUE, extrapolation_type = "linear")


  ## Extract Urban share
  urban_share <- ssp_data[, , "Population|Urban|Share"]
  pattern <- "_v9_130115"
  getNames(urban_share, dim = 2) <- gsub(pattern = pattern, replacement = "", x = getNames(urban_share, dim = 2))

  ## Collapse names to remove names which are not needed
  urban_share <- collapseNames(urban_share)


  ## use population datata
  population <- ssp_data[, , "Population"]

  ## Extract data using pattern (the same as we used in urban share) - For consistency
  population <- collapseNames(population[, , grep(pattern = "_v9_130115", x = getNames(population, dim = 2), value = TRUE)])

  ## Remove pattern (the same as we used in urban share) - for consistency
  getNames(population, dim = 2) <- gsub(pattern = pattern, replacement = "", x = getNames(population, dim = 2))

  ## Use NCAR data as this is the only source of urban share - for consistency
  population <- collapseNames(population[, , "NCAR"])

  ## Attach a fatal error in case names don't macth due to changes in parent functions
  if (length(setdiff(getNames(population), getNames(urban_share))) != 0) stop("Unable to find matching names for population and urban share data. Stopping here to avoid incorrect calculations")

  ## Absolute Urban population living in cities
  urban_population <- population * urban_share / 100

  ## harmonize till 2020
  urban_population[, paste0("y", seq(1995, 2020, 5)), ] <- urban_population[, paste0("y", seq(1995, 2020, 5)), "SSP2"]

  ## Correct SSP sequence
  urban_population <- urban_population[, , paste0("SSP", 1:5)]

  max_urban_pop <- urban_population[, 1, ]
  max_urban_pop[max_urban_pop != 0] <- 0
  getYears(max_urban_pop) <- NULL

  ## Calculate additional wood demand based on Churkina et al 2020
  ## https://doi.org/10.1038/s41893-019-0462-4
  ## See SI page 8, part on "Calculations of carbon storage in timber buildings and respective demand for timber"
  ## 35 year demand = (pop diff in 2050-2105) * McW * CW * PR
  ## McW is from Supplementary Table 3   and Supplementary Table 4)
  ## CW is 0.5 (carbon in dry matter)
  ## PR is papers scenarios 0.1,0.5,0.9 etc

  ## Create MWC --- See Table 3 and Table 4 in SI https://doi.org/10.1038/s41893-019-0462-4
  ## Using Mean values
  MwC <- data.frame(var = c("prim_timber", "encl_timber", "encl_wood_fiber"), val = c(5942.50, 1104.53, 391.98))
  MwC <- as.magpie(MwC)

  ## Create CW i.e. Kg C per Kg of wood
  ## See: Martin, A. R., Doraisami, M. & Thomas, S. C. Global patterns in wood carbon concentration across the world’s trees and forests. Nature Geoscience 11, 915-920 (2018).
  ## Using global average of 0.476 +- 0.04
  CW <- 0.5

  ## Create PR i.e. the scenarios for which these demand are generated
  PR <- data.frame(scen = c("BAU", "10pc", "50pc", "90pc"), val = c(5, 10, 50, 90) / 100)
  PR <- as.magpie(PR)

  ## We now start filling values from 2020 onward
  demand_years <- paste0("y", seq(2015, 2100, 5))

  ## Demand Urban population change between 2100 and 2080
  building_wood_demand <- NULL
  # share <- 1/(as.numeric(gsub(pattern = "y",x = demand_years,replacement = ""))[length(demand_years)]-as.numeric(gsub(pattern = "y",x = demand_years,replacement = ""))[2])
  # for (i in 2:length(demand_years)) {
  #  temp_demand <- urban_population[, demand_years[i], ] * share * dimSums(MwC,dim=3) * CW * PR ## Millio Cap * kg / cap  * C * PR = Million Kg
  #  building_wood_demand <- mbind(building_wood_demand, temp_demand)
  # }
  all_annual_years <- paste0("y", seq(1995, 2100, 1))
  urban_population_annual <- time_interpolate(
    dataset = urban_population,
    interpolated_year = setdiff(all_annual_years, getYears(urban_population)),
    integrate_interpolated_years = TRUE,
    extrapolation_type = "constant"
  )

  ## Non-zero-urban pop countries
  valid_countries <- unique(where(urban_population>0)$true$regions)
  
  for (i in valid_countries) {
    cat(i,"\n")
    ssp_temp_demand <- NULL
    for (j in paste0("SSP", 1:5)) {
      max_pop <- max(urban_population[i, , j][,paste0("y",seq(1995,2015,5)),,invert=TRUE])
      pop_diff <- setYears(max_pop - urban_population[i, "y2020", j], NULL)
        peak_year <- as.numeric(gsub(pattern = "y", replacement = "", x = as.character(where(urban_population[i, , j] == max_pop)$true$years)))
        distribution_length <- peak_year - 2020
        peak_building_demand <- pop_diff * dimSums(MwC, dim = 3) * CW * PR ## Million Cap * kg / cap  * C * PR = Million kg C
        
        share_df <- data.frame(
          region = i,
          year = 2021:(2021 + distribution_length - 1),
          share = (1:distribution_length) / sum(1:distribution_length))
        
        dist_share_magpie <- collapseNames(as.magpie(share_df, spatial = , temporal = "year"))
        
        temp_demand <- peak_building_demand * dist_share_magpie 
        
        if (peak_year < 2100) {
          remaining_years <- seq(peak_year + 1, 2100, 1)
          temp_df <- data.frame(
            region = i,
            year = remaining_years,
            val = 0
          )
          temp2 <- setNames(as.magpie(temp_df, temporal = 2), j)
          temp2 <- add_dimension(x = temp2, dim = 3.2, nm = getNames(temp_demand, dim = 2))
          temp2[,remaining_years,] <- temp_demand[,peak_year,]
          if(as.numeric(pop_diff)==0){
            temp_demand <- temp2
            temp_demand[temp_demand!=0] <- 0
          } else temp_demand <- mbind(temp_demand, temp2)
        }
        ssp_temp_demand <- mbind(ssp_temp_demand,temp_demand)
    }
    building_wood_demand <- mbind(building_wood_demand, ssp_temp_demand)
  }

  ## Convert to Million tC
  building_wood_demand <- building_wood_demand / 1e3
  # print(dimSums(building_wood_demand[,,"SSP2"],dim=1))
  ## Convert to Million tDM <---- Instead of doing this step one can simple skip multiplication with CW inside the loop
  ## We also multiply by 2 as Churkina et al 2020 state that you can assume 50% harvest loss so for every ton of material
  ## in wood we'd actually harvest 2 tons. This is already an extreme scenario as we don't have this much harvest losses.

  building_wood_demand <- 2 * building_wood_demand / CW
  # print(dimSums(building_wood_demand[,,"SSP2"],dim=1))

  ## Distribute shock over time

  out <- toolCountryFill(x = building_wood_demand[,intersect(getYears(urban_population),getYears(building_wood_demand)),],fill = 0)

  return(list(
    x = out,
    weight = NULL,
    min = 0,
    unit = "mio m3",
    description = "Calculates the wood demand based on historical FAO data"
  ))
}
