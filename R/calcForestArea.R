#' @title calcForestArea
#' @description Calculates consistent forest area and its subcategories based on FAO_FRA2015
#' and LanduseInitialisation data.
#'
#' @param selectyears defaults to past
#' @return List of magpie object with results on country level, weight, unit and description.
#' @author Kristine Karstens, Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' calcOutput("ForestArea")
#' }
#' @export

calcForestArea <- function(selectyears = "past") {

  years <- sort(findset(selectyears, noset = "original"))

  forest   <- readSource("FAO_FRA2015", "fac")[, , c("Forest", "NatFor", "PrimFor", "NatRegFor", "PlantFor")]

  # Plantation data is bit strange in FRA2015, we update this with FRA2020 data (but only till 2015)
  # We do this because FRA2020 has stopped reporting separately on primf and secdf
  # but we can still use data for planted forest

  ## Overall FRA 2020 data
  fra2020      <- readSource("FRA2020", "forest_area")

  ## Find which year is missing in FRA2020 data (which exisits in FRA2015)
  missingYears <- setdiff(getYears(forest), getYears(fra2020))

  ## Linear interpolation to missing year
  fra2020      <- time_interpolate(dataset = fra2020,
                                   interpolated_year = missingYears,
                                   integrate_interpolated_years = TRUE,
                                   extrapolation_type = "linear")

  ## Replace FRA2015 planted forest data with FRA 2020 data
  forest[, , "PlantFor"] <- fra2020[, getYears(forest), "plantedForest"]

  # As planted forest data is now different, we need to update overall forest area
  # (sum of nat.reg.forest and planted forest)
  forest[, , "Forest"]   <- forest[, , "NatFor"] + forest[, , "PlantFor"]

  forest   <- time_interpolate(forest, interpolated_year = years, integrate_interpolated_years = TRUE,
                               extrapolation_type = "constant")[, years, ]
  vcat(verbosity = 3, "Forest is interpolated for missing years and held constant for the period before FAO starts")

  ### fix know issues

  forest["HND", , "PlantFor"]   <- forest["HND", , "Forest"] - forest["HND", , "NatFor"]
  forest["IDN", , "Forest"]     <- forest["IDN", , "NatFor"] + forest["IDN", , "PlantFor"]
  forest["FIN", , "NatRegFor"]  <- forest["FIN", , "NatFor"] - forest["FIN", , "PrimFor"]
  forest["PSE", , "PlantFor"]   <- 2 / 3 * forest["PSE", , "Forest"]
  forest["PSE", , "NatRegFor"]  <- 1 / 3 * forest["PSE", , "Forest"]

  ### fixing inconsistencies assuming total forest areas and shares of subcategories are reported correctly

  forestSumSub                  <- dimSums(forest[, , c("NatFor", "PlantFor")], dim = 3)
  forest[, , "PlantFor"]        <- toolNAreplace(forest[, , "PlantFor"] /
                                                   forestSumSub * setNames(forest[, , "Forest"], NULL))$x
  forest[, , "NatFor"]          <- toolNAreplace(forest[, , "NatFor"] /
                                                   forestSumSub * setNames(forest[, , "Forest"], NULL))$x

  forestSumSubSub               <- dimSums(forest[, , c("PrimFor", "NatRegFor")], dim = 3)
  forest[, , "PrimFor"]         <- toolNAreplace(forest[, , "PrimFor"] /
                                                   forestSumSubSub * setNames(forest[, , "NatFor"], NULL))$x
  forest[, , "NatRegFor"]       <- toolNAreplace(forest[, , "NatRegFor"] /
                                                   forestSumSubSub * setNames(forest[, , "NatFor"], NULL))$x

  ###########################
  # fixing missing data on split between PrimFor (primforest), NatRegFor (secdforest)
  # and PlantFor (forestry) with LUH data

  luh <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE, selectyears = selectyears,
                    cells = "lpjcell", cellular = FALSE, aggregate = FALSE)[, , c("primf", "secdf")]

  secondaryForest <- luh[, , "secdf"] - setNames(forest[, getYears(luh), "PlantFor"], NULL)
  if (any(secondaryForest < 0)) {
    tmp <- secondaryForest
    tmp[tmp > 0] <- 0
    tmp <- dimSums(tmp, dim = 1)
    vcat(verbosity = 2, paste("Mismatch of FAO forestry and Hurtt secondary forest:",
                              paste(paste(getYears(tmp), round(tmp, 0), "Mha, "), collapse = " "), ". cut off."))
    secondaryForest[secondaryForest < 0] <- 0
  }
  forestry <- luh[, , "secdf"] - secondaryForest

  luhForest <- mbind(setNames(forestry, "PlantFor"),
                     setNames(luh[, , c("primf")], "PrimFor"),
                     setNames(secondaryForest, "NatRegFor")) + 10^-10
  # 10^-10 added to allow share estimation even under missing area information
  luhForestShare <- luhForest / dimSums(luhForest, dim = 3)
  luhNatForestShare <- luhForest[, , c("PrimFor", "NatRegFor")] / dimSums(luhForest[, , c("PrimFor", "NatRegFor")],
                                                                          dim = 3)

  miss         <- where(round(dimSums(forest[, , c("NatFor", "PlantFor")], dim = 3), 6) !=
                          round(forest[, , "Forest"], 6))$true$regions

  if (length(miss) > 0) {
    forest[miss, , c("PlantFor", "PrimFor", "NatRegFor")] <- luhForestShare[miss, , ] *
      setNames(forest[miss, , "Forest"], NULL)
    forest[miss, , "NatFor"] <- setNames(forest[miss, , "PrimFor"] + forest[miss, , "NatRegFor"], NULL)
  }

  miss         <- where(round(dimSums(forest[, , c("PrimFor", "NatRegFor")], dim = 3), 6) !=
                          round(forest[, , "NatFor"], 6))$true$regions
  if (length(miss > 0)) {
    forest[miss, , c("PrimFor", "NatRegFor")] <- luhNatForestShare[miss, , ] * setNames(forest[miss, , "NatFor"], NULL)
  }

  ####################################

  map <- data.frame(fao    = c("Forest", "NatFor",     "PrimFor",    "NatRegFor",  "PlantFor"),
                    magpie = c("forest", "natrforest", "primforest", "secdforest", "forestry"))
  out <- toolAggregate(forest, map, from = "fao", to = "magpie", dim = 3)

  if (max(abs(dimSums(out[, , c("natrforest", "forestry")]) - out[, , "forest"])) > 10^-6 ||
      max(abs(dimSums(out[, , c("secdforest", "primforest")], dim = 3) - out[, , "natrforest"])) > 10^-6) {
    warning("There are inconsistencies within the forest area data set.")
  }

  return(list(x = out,
              weight = NULL,
              unit = "Mha",
              description = "Forest are and its subcategories")
  )
}
