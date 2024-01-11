#' @title calcFallowLand
#' @description
#' Calculates fallow land on grid cell level,
#' based on physical cropland extend and harvested area output
#' of LandInG data.
#' The formula
#' "fallow land are = max( physical cropland area - harvested cropland area, 0)"
#' is used.
#' Due to multiple cropping, harvested cropland area can be greater than non-fallow land area
#' and even greater than physical cropland area.
#' Thus, the results can only be considered a rough estimate of fallow land area.
#' @param cellular TRUE for cellular outputs.
#' @param years "all" or NULL for all years available
#' @return MAgPIE object containing fallow land in Mha
#' @author David Hoetten, Felicitas Beier
#' @seealso
#' \code{\link{readLandInG}}
#' @examples
#' \dontrun{
#' calcOutput("FallowLand")
#' }
#' @importFrom magclass dimSums mbind
#' @importFrom madrat toolConditionalReplace
#' @importFrom magpiesets findset
#'
calcFallowLand <- function(cellular = TRUE, years = "past") {

  if (years == "past") {
    years <- findset("past")
  } else if (years == "all") {
    years <- NULL
  }

  harvestedArea <- readSource("LandInG", subtype = "harvestedArea") [, years, ]

  harvestedAreaCrops <- harvestedArea[, , c("pasture"), invert = TRUE]

  physicalArea <- readSource("LandInG", subtype = "physicalArea") [, years, ]

  fallowLand <- dimSums(physicalArea, "irrigation") -
    dimSums(harvestedAreaCrops, c("irrigation", "crop"))

  fallowLand <- toolConditionalReplace(fallowLand, conditions = c("<0"), replaceby = 0)

  # Aggregation to iso-level
  if (!cellular) {
    # aggregate to countries
    fallowLand <- dimSums(fallowLand, dim = c("x", "y"))
    # fill missing countries with 0
    fallowLand <- toolConditionalReplace(x = toolCountryFill(fallowLand),
                                         conditions = "is.na()", replaceby = 0)
  }

  return(list(x = fallowLand,
              weight = NULL,
              description = "Fallow land",
              unit = "Mha",
              isocountries = FALSE))

}
