#' Convert subtypes of the PBL_MACC_2022 data
#'
#' Convert subtypes from PBL_MACC_2022 to data on ISO country level.#'
#'
#' @param x MAgPIE object containing PBL_MACC_2022 data on region level
#' @return PBL_MACC_2022 data as MAgPIE object for all subtypes aggregated to
#' country level
#' @author Michael Windisch, Florian Humpenoeder
#' @seealso [madrat::readSource()]

convertPBL_MACC_2022 <- function(x) { # nolint : object_name_linter.

  map <- toolGetMapping(type = "regional", name = "regionmapping_IMAGE_PBL_MACC_2019.csv", where = "mappingfolder")

  y <- toolAggregate(x, map, from = "RegionCode", to = "CountryCode")
  y <- toolCountryFill(y, fill = 0)

  return(y)
}
