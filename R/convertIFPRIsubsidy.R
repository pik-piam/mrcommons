#' @title convertIFPRIsubsidy
#' @description disaggregates EU subsidies for crops and livestock from IFPRI table to country level
#'
#' @param x magpie object provided by the read function
#' @return magpie object of agricultural subsidies
#' @author Debbora Leip
#' @importFrom readxl read_excel
#' @examples
#' \dontrun{
#' a <- readSource("IFPRIsubsidy", convert = TRUE)
#' }
#'
convertIFPRIsubsidy <- function(x) {
  # create mapping to disaggregate EU
  h12 <- toolGetMapping("regionmappingH12.csv", type = "spatial", where = "mappingfolder")
  h12$RegionCode[h12$RegionCode != "EUR"] <- h12$CountryCode[h12$RegionCode != "EUR"]
  h12 <- h12[h12$RegionCode %in% getItems(x, dim = 1), ]

  # FAO land as disaggregation weight
  years <- getItems(x, dim = 2)
  regions <- h12$CountryCode
  weight <- calcOutput("FAOLand", aggregate = FALSE)[regions, years, "6610|Agricultural land"]

  xCrops <- toolAggregate(x[, , "Crops"], rel = h12, weight = weight, from = "RegionCode", to = "CountryCode")
  xLivs <- toolAggregate(x[, , "Livestock"], rel = h12, weight = weight, from = "RegionCode", to = "CountryCode")
  xNal <- toolAggregate(x[, , "Non-Allocated"], rel = h12, weight = weight, from = "RegionCode", to = "CountryCode")

  # put back into single MAgPIE object
  x <- mbind(xCrops, xLivs, xNal)
  x <- toolCountryFill(x, fill = 0)
  x[is.na(x)] <- 0

  # convert to USDMER05 (for years with missing conversion factors we assume no inflation)
  x <- GDPuc::convertGDP(x,
                         unit_in = "current US$MER",
                         unit_out = "constant 2005 US$MER", replace_NAs = "no_conversion"
  )

  return(x)
}
