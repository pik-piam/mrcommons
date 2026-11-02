#' @title calcBEYield
#' @description Calculates bioenergy crop yields from Li et al. (2020) for the two
#' MAgPIE bioenergy types begr and betr. begr takes the maximum yield across
#' Miscanthus and Switchgrass per cell. betr takes the maximum yield across
#' Eucalypt (warm/tropical climates only), Poplar, and Willow per cell.
#'
#' @param returnWeights if TRUE returns crop-specific cellular cropland area weights
#'   (begr/betr) instead of the yields
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso [readLi2020()]
#' @examples
#' \dontrun{
#' calcOutput("BEYield", aggregate = FALSE)
#' calcOutput("BEYield", returnWeights = TRUE, aggregate = FALSE)
#' }
calcBEYield <- function(returnWeights = FALSE) {

  x <- setYears(readSource("Li2020", convert = FALSE), NULL)

  climateWarm <- readSource("IPCCClimate", convert = "onlycorrect")

  selectionWarm <- c(
    "Warm Temperate Moist" = TRUE,
    "Warm Temperate Dry"   = TRUE,
    "Cool Temperate Moist" = FALSE,
    "Cool Temperate Dry"   = FALSE,
    "Polar Moist"          = FALSE,
    "Polar Dry"            = FALSE,
    "Boreal Moist"         = FALSE,
    "Boreal Dry"           = FALSE,
    "Tropical Montane"     = TRUE,
    "Tropical Wet"         = TRUE,
    "Tropical Moist"       = TRUE,
    "Tropical Dry"         = TRUE
  )

  # each cell has exactly one zone = 1, so summing over TRUE zones gives 0 or 1
  climateWarm <- dimSums(climateWarm[, , names(selectionWarm)[selectionWarm]], dim = 3.1)

  # begr: cell-wise maximum across herbaceous crops; na.rm = TRUE retains cells
  # where only one of the two species has a valid yield
  begr <- setNames(pmax(x[, , "Miscanthus"], x[, , "Switchgrass"], na.rm = TRUE), "begr")

  # Eucalyptus is only viable in warm/tropical climates. Mask its yield to NA in
  # cold cells so pmax (with na.rm = TRUE) falls back to Poplar/Willow there,
  # while warm cells still consider all three species.
  # betr: cell-wise maximum across woody crops; same na.rm rationale
  eucalypt <- x[, , "Eucalypt"]
  eucalypt[climateWarm == 0] <- NA
  betr <- setNames(pmax(eucalypt, x[, , "Poplar"], x[, , "Willow"], na.rm = TRUE), "betr")

  out <- mbind(begr, betr)

  # Cropland area (summed over all crops, 1995 base year) used as aggregation weight.
  weight <- calcOutput("Croparea", sectoral = "kcr", physical = TRUE, irrigation = TRUE,
                       cellular = TRUE, aggregate = FALSE)
  weightDetail <- setYears(dimSums(weight[, "y1995", ], dim = "MAG"), NULL)
  weight       <- setYears(dimSums(weight[, "y1995", ], dim = 3), NULL)

  # Crop-specific weights: cells with NA yield are set to 0 so they are
  # excluded from weighted aggregation and flagged via zeroWeight = "setNA".
  weightBegr <- weight
  weightBegr[is.na(out[, , "begr"])] <- 0
  weightDetailBegr <- weightDetail
  weightDetailBegr[is.na(out[, , "begr"])] <- 0
  weightBetr <- weight
  weightBetr[is.na(out[, , "betr"])] <- 0
  weightDetailBetr <- weightDetail
  weightDetailBetr[is.na(out[, , "betr"])] <- 0
  weightExp <- mbind(setNames(weightBegr, "begr"), setNames(weightBetr, "betr"))
  weightDetailExp <- mbind(add_dimension(weightDetailBegr, dim = 3.1,
                                         add = "kbe", nm = "begr"),
                           add_dimension(weightDetailBetr, dim = 3.1,
                                         add = "kbe", nm = "betr"))
  out[is.na(out)] <- 0

  if (returnWeights) {
    return(list(x            = weightDetailExp,
                weight       = NULL,
                unit         = "Mha",
                description  = paste("Cellular cropland area weights (y1995 cropland area,",
                                     "timeless) used for aggregation of bioenergy crop",
                                     "yields from Li et al. (2020)"),
                isocountries = FALSE))
  }

  return(list(x            = out,
              weight       = weightExp,
              unit         = "t DM ha-1 yr-1",
              description  = "Bioenergy crop yields from Li et al. (2020)",
              isocountries = FALSE))
}
