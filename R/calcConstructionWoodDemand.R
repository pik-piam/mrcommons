#' @title calcConstructionWoodDemand
#' @description
#' Calculates the demand of construction wood from Galina et al. 2020 data.
#' See
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' [calcFAOmassbalance_pre()]
#' @examples
#' \dontrun{
#' calcOutput("ConstructionWoodDemand")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @export

calcConstructionWoodDemand <- function() {

  ## Get Urban population SSP scenarios
  urbanPopulation <- calcOutput("Urban",
                                scenario = "SSPs",
                                asShare = FALSE,
                                naming = "scenario",
                                years = seq(1995, 2100, 5),
                                aggregate = FALSE)

  ## Calculate additional wood demand based on Churkina et al 2020
  ## https://doi.org/10.1038/s41893-019-0462-4
  ## See SI page 8, part on "Calculations of carbon storage in timber buildings and respective demand for timber"
  ## 35 year demand = (pop diff in 2050-2105) * McW * CW * PR
  ## McW is from Supplementary Table 3   and Supplementary Table 4)
  ## CW is 0.5 (carbon in dry matter)
  ## PR is papers scenarios 0.1,0.5,0.9 etc

  ## Create MWC --- See Table 3 and Table 4 in SI https://doi.org/10.1038/s41893-019-0462-4
  ## Using Mean values
  mwc <- data.frame(var = c("prim_timber", "encl_timber", "encl_wood_fiber"), val = c(5942.50, 1104.53, 391.98))
  mwc <- as.magpie(mwc)

  ## Create CW i.e. Kg C per Kg of wood
  ## See: Martin, A. R., Doraisami, M. & Thomas, S. C. Global patterns in wood
  ## carbon concentration across the world’s trees and forests.
  ## Nature Geoscience 11, 915-920 (2018).
  ## Using global average of 0.476 +- 0.04
  cw <- 0.5

  ## Create PR i.e. the scenarios for which these demand are generated
  pr <- data.frame(scen = c("BAU", "10pc", "50pc", "90pc"), val = c(5, 10, 50, 90) / 100)
  pr <- as.magpie(pr)

  ## Demand Urban population change between 2100 and 2080
  buildingWoodDemand <- NULL

  ## Non-zero-urban pop countries
  validCountries <- unique(where(urbanPopulation > 0)$true$regions)

  for (i in validCountries) {
    ## Set SSP dummy demand to NULL after every loop

    sspTempDemand <- NULL
    for (j in paste0("SSP", 1:5)) {
      ## Check the max population for whole duration.
      ## Remember - We don't need to build more wooden buildings than would be needed by peak urban population
      maxPop <- max(urbanPopulation[i, , j][, paste0("y", seq(1995, 2015, 5)), , invert = TRUE])

      ## Check the difference between current (2020) urban population with peak urban population
      popDiff <- setYears(maxPop - urbanPopulation[i, "y2020", j], NULL)

      ## Find the peak year
      peakYear <- as.numeric(gsub(pattern = "y", replacement = "",
                                  x = as.character(where(urbanPopulation[i, , j] == maxPop)$true$years)))
      ## Take the first peak year in case of multiple
      peakYear <- peakYear[1]

      ## Calculate the time frame between peak urban population and current time period (2020)
      distributionLength <- peakYear - 2020

      ## Check what would be the demand at peak. Remember that population is a stock resource.
      ## Also keep in mind that this is the demand we calculate for new people moving in cities not overall population
      peakBuildingDemand <- popDiff * dimSums(mwc, dim = 3) * cw * pr ## Million Cap * kg / cap  * C * PR = Million kg C

      ## Calculate how the distribution share should look like. Higher weight to later years
      ## This distribution is a linear 45 degree line
      shareDf <- data.frame(
        region = i,
        year = 2021:(2021 + distributionLength - 1),
        share = (1:distributionLength) / sum(1:distributionLength)
      )

      ## Create magpie object for share
      distShareMagpie <- collapseNames(as.magpie(shareDf, spatial = , temporal = "year"))

      ## Distribute the peak demand to annual numbers
      tempDemand <- peakBuildingDemand * distShareMagpie

      #### Additional checks for correct calculations :

      ## If peak appears before 2100, no more building wood is to be demanded

      if (peakYear < 2100) {
        ## Find out how many years left
        remainingYears <- seq(peakYear + 1, 2100, 1)
        tempDf <- data.frame(
          region = i,
          year = remainingYears,
          val = 0
        )
        ## Create magpie object for this region
        temp2 <- setNames(as.magpie(tempDf, temporal = 2), j)
        ## Create demand scenarios
        temp2 <- add_dimension(x = temp2, dim = 3.2, nm = getNames(tempDemand, dim = 2))
        ## Fill remaining years with peak demand numbers
        temp2[, remainingYears, ] <- tempDemand[, peakYear, ]
        if (as.numeric(popDiff) == 0) { ## If peak occurred in 2020 then no new buildings needed (?)
          tempDemand <- temp2
          tempDemand[tempDemand != 0] <- 0
        } else {
          tempDemand <- mbind(tempDemand, temp2) ##
        }
      }
      ## Merge demands together -- SSP specific, one region, looped over all SSPS
      sspTempDemand <- mbind(sspTempDemand, tempDemand)
    }
    ## All SSPs for each country at this stage, looped over all countries
    buildingWoodDemand <- mbind(buildingWoodDemand, sspTempDemand)
  }

  ## Convert to Million tC from Million kgC (coming from loop)
  buildingWoodDemand <- buildingWoodDemand / 1e3

  ## Convert to Million tDM <---- Instead of doing this step one can simple skip
  ## multiplication with CW inside the loop.
  ## We also multiply by 2 as Churkina et al 2020 state that you can assume 50%
  ## harvest loss so for every ton of material in wood we'd actually harvest 2 tons.
  ## This is already an extreme scenario as we don't have this much harvest losses.
  buildingWoodDemand <- 2 * buildingWoodDemand / cw

  out <- suppressMessages(
    toolCountryFill(
      x = buildingWoodDemand[, intersect(getYears(urbanPopulation), getYears(buildingWoodDemand)), ],
      fill = 0
    )
  )

  out[, , "BAU"] <- 0 ## No constr wood demand in BAU case

  return(list(
    x = out,
    weight = NULL,
    min = 0,
    unit = "mio tDM",
    description = paste("Calculates the construction wood demand for a certain",
                        "share of NEW urban settlers based on SSP urban population",
                        "and Churkina et al 2020. Calculated based on current",
                        "urban population and peak urban population.")
  ))
}
