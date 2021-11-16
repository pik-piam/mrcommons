#' @title calcAdjustGrassi2021
#' @description
#' Calculates the carbon emission adjustement factors as derived by Grassi et al. 2021
#' Adjustement factors are provided in GtCO2 yr-1. Positive and negative values possible.
#'
#' @return magpie object with emission adjustement factors weighted by country C removals 2000 to 2015.
#' @author Michael Windisch, Florian Humpenoeder
#' @examples
#'
#' \dontrun{
#' calcOutput("AdjustGrassi2021")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @importFrom madrat toolGetMapping
#' @export

calcAdjustGrassi2021 <- function(){
  df <- readSource("AdjustGrassi2021",subtype="data")

  wf <- readSource("AdjustGrassi2021",subtype="weight")
  wf <- dimSums(wf,dim=2)/dim(wf)[2]
  wf <- toolCountryFill(wf,fill=0)
  wf <- abs(wf)

  mf <- toolGetMapping(type = "regional", name = "IPCC_AR6_10region.csv")

  # downscale from 10 IPCC regions to country level using country removals as weight
  x <- toolAggregate(df, rel = mf, weight = wf, dim = 1, partrel = F, from = "RegionCode", to = "CountryCode")


  return(list(x=x,
              weight=wf,
              unit="GtCO2 yr-1",
              description="Emission adjustement factors")
  )
}
