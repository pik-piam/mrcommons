#' @title calcLanduseInitialisation
#' @description Calculates the cellular MAgPIE landuse initialisation area. Data from FAO on forestry is used to split the secondary forest pool of the LU2v2 dataset into forestry and secd_forest.
#'
#' @param cellular cellular (TRUE) or country-level/regional (FALSE) data? For country-level vs regional data, remember to set "aggregate" to false.
#' @param nclasses options are either "six", "seven" or "nine".
#' \itemize{
#' \item "six" includes the original land use classes "crop", "past", "forestry", "forest", "urban" and "other"
#' \item "seven" separates primary and secondary forest and includes "crop", "past", "forestry", "primforest", "secdforest", "urban" and "other"
#' \item "nine" adds the separation of pasture and rangelands, as well as a differentiation of primary and secondary non-forest vegetation and therefore returns "crop", "past", "range", "forestry", "primforest", "secdforest", "urban", "primother" and "secdother"
#' }
#' @param fao_corr if TRUE forest area is corrected with FAO data.
#' @param cells    if cellular is TRUE: "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @param input_magpie applies area fix (set cell with zero area to minimal value to not disturb aggregating to clusters)
#' @param selectyears default on "past"
#' @return List of magpie object with results on country or cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens, Felcitas Beier, Patrick v. Jeetze, Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' calcOutput("LanduseInitialisation")
#' }
#' @importFrom magclass setNames where


calcLanduseInitialisation <- function(cellular = FALSE, nclasses = "seven", fao_corr = TRUE, cells = "magpiecell", selectyears = "past", input_magpie = FALSE) {
  selectyears <- sort(findset(selectyears, noset = "original"))

  # define output list
  .out <- function(x, cellular) {
    return(list(
      x = x,
      weight = NULL,
      unit = "Mha",
      min = 0,
      max = 14900, ### global land area
      description = "Land use initialisation data for different land pools",
      isocountries = !cellular
    ))
  }

  # handle some common data postprocessings (input_magpie and nclasses) by reading in
  # data from a default case (input_magpie = FALSE, nclasses = "nine") and put these computations on top
  if (isTRUE(input_magpie) || nclasses != "nine") {
    out <- calcOutput("LanduseInitialisation", cellular = cellular, nclasses = "nine", fao_corr = fao_corr,
                      cells = cells, selectyears = selectyears, input_magpie = FALSE, aggregate = FALSE)

    if (nclasses != "nine") {
      map <- data.frame(nine  = c("crop", "past", "range", "forestry", "primforest", "secdforest", "urban", "primother", "secdother"),
                        seven = c("crop", "past", "past", "forestry", "primforest", "secdforest", "urban", "other", "other"),
                        six   = c("crop", "past", "past", "forestry", "forest", "forest", "urban", "other", "other"))
      if (!(nclasses %in% names(map))) stop("unknown nclasses setting \"", nclasses, "\"")
      out <- toolAggregate(out, rel = map, dim = 3, from = "nine", to = nclasses)
    }

    if (isTRUE(input_magpie)) {
      # add some small area to completely empty cells to avoid
      # problems in the further processing
      out <- round(out, 8)
      cellArea <- dimSums(out, dim = 3)
      out[, , "secdother"][cellArea == 0] <- 10^-6
    }
    return(.out(out, cellular))
  }

  ##### Here starts the core part of this function #####

  if (cellular == FALSE) {
    luh2v2 <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE, selectyears = selectyears, cells = cells, cellular = FALSE, aggregate = FALSE)

    if (fao_corr == FALSE) {

      # divide secondary forest into forestry and secdf.
      forestry_country <- readSource("FAO_FRA2015", "fac")[, , "PlantFor"]
      forestry_country <- time_interpolate(forestry_country, interpolated_year = selectyears, integrate_interpolated_years = TRUE, extrapolation_type = "constant")
      vcat(verbosity = 3, "Forestry is interpolated for missing years and held constant for the period before FAO starts")

      secondary_forest <- luh2v2[, , "secdf"] - setNames(forestry_country[, getYears(luh2v2), ], NULL)
      forestry_overflow <- secondary_forest
      forestry_overflow[forestry_overflow > 0] <- 0
      tmp <- dimSums(forestry_overflow, dim = 1)
      if (any(tmp < 0)) {
        vcat(verbosity = 2, paste("Mismatch of FAO forestry and Hurtt secondary forest:", paste(paste(getYears(tmp), round(tmp, 0), "Mha, "), collapse = " "), ". cut off."))
      }
      secondary_forest[secondary_forest < 0] <- 0
      forestry <- luh2v2[, , "secdf"] - secondary_forest

      # calculate other landpools
      crop <- dimSums(luh2v2[, , c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")], dim = 3)

      out <- mbind(
        setNames(crop, "crop"),
        setNames(luh2v2[, , c("pastr")], "past"),
        setNames(luh2v2[, , c("range")], "range"),
        setNames(forestry, "forestry"),
        setNames(luh2v2[, , c("primf")], "primforest"),
        setNames(secondary_forest, "secdforest"),
        setNames(luh2v2[, , c("urban")], "urban"),
        setNames(luh2v2[, , c("primn")], "primother"),
        setNames(luh2v2[, , c("secdn")], "secdother"))

    } else if (fao_corr == TRUE) {
      FAOforest <- calcOutput("ForestArea", selectyears = selectyears, aggregate = FALSE)

      other <- dimSums(luh2v2[, , c("primn", "secdn")], dim = 3)
      forest <- dimSums(luh2v2[, , c("primf", "secdf")], dim = 3)

      # Correct for overflow effects (FAO_forest greater than forest and other land available in LUH)
      overflow <- FAOforest[, , "forest"] - (forest + other)
      overflow[overflow < 0] <- 0
      if (any((of <- dimSums(overflow, dim = 1)) > 0)) {
        vcat(verbosity = 2, paste("Mismatch of FAO forest exceed LUH forest + other land by:", paste(paste(getYears(of), round(of, 0), "Mha, "), collapse = " "), ". FAO forest data will be cut."))
      }

      # corrected forest areas <- weight of forest subcategories * corrected total forest area
      FAOforest <- FAOforest / setNames(FAOforest[, , "forest"], NULL) * setNames(FAOforest[, , "forest"] - overflow, NULL)
      FAOforest <- toolNAreplace(FAOforest)$x

      # Correct other land as diff of FAO forest area and LUH forest area
      FAOother <- other + forest - FAOforest[, , "forest"]
      cat("Reallocation to other land results in extremely low negative values for other land in", where(FAOother < 0)$true$regions, "with a range of", range(FAOother[FAOother < 0]))
      cat("\nSuch values are replaced with 0.")
      FAOother[FAOother < 0] <- 0

      # calculate other landpools
      crop <- dimSums(luh2v2[, , c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")], dim = 3)
      if (nclasses %in% c("seven", "six")) {
        pasture <- dimSums(luh2v2[, , c("pastr", "range")], dim = 3)
      }

      # differentiate corrected other land into primary and secondary
      totother_luh <- dimSums(luh2v2[, , c("primn", "secdn")], dim = 3)
      primother_shr <- luh2v2[, , "primn"] / setNames(totother_luh + 1e-10, NULL)
      secdother_shr <- luh2v2[, , "secdn"] / setNames(totother_luh + 1e-10, NULL)
      # where luh2 does not report other land, but we find other land after
      # reallocation set share of secondary other land to 1
      secdother_shr[secdother_shr == 0 & primother_shr == 0] <- 1
      # multiply shares of primary and secondary non-forest veg with corrected other land
      primother <- primother_shr * setNames(FAOother, NULL)
      secdother <- secdother_shr * setNames(FAOother, NULL)

      out <- mbind(
        setNames(crop, "crop"),
        setNames(luh2v2[, , c("pastr")], "past"),
        setNames(luh2v2[, , c("range")], "range"),
        setNames(luh2v2[, , c("urban")], "urban"),
        FAOforest[, , c("primforest", "secdforest", "forestry")],
        setNames(primother, "primother"),
        setNames(secdother, "secdother"))
    }
  } else {
    if (fao_corr == FALSE) {

      # Load cellular and country data
      countrydata <- calcOutput("LanduseInitialisation", aggregate = FALSE, nclasses = "seven", fao_corr = FALSE, selectyears = selectyears, cellular = FALSE)
      luh2v2 <- calcOutput("LUH2v2", aggregate = FALSE, cells = cells, landuse_types = "LUH2v2", irrigation = FALSE, cellular = TRUE, selectyears = selectyears)
      luh2v2 <- toolCell2isoCell(luh2v2, cells = cells)
      countries <- getItems(luh2v2, dim = 1.1)
      mapping   <- data.frame(iso = getItems(luh2v2, dim = 1.1, full = TRUE), celliso = getItems(luh2v2, dim = 1))

      # divide secondary forest into forestry and secdf.
      forestry_shr <- countrydata[, , "forestry"] / dimSums(countrydata[, , c("forestry", "secdforest")], dim = 3)
      forestry_shr[is.nan(forestry_shr)] <- 0
      forestry_shr <- toolAggregate(x = forestry_shr[countries, , ], rel = mapping, from = "iso", to = "celliso", dim = 1)
      forestry <- forestry_shr * luh2v2[, , "secdf"]
      secondary_forest <- (1 - forestry_shr) * luh2v2[, , "secdf"]

      # calculate other landpools
      crop <- dimSums(luh2v2[, , c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")], dim = 3)

      out <- mbind(
        setNames(crop, "crop"),
        setNames(luh2v2[, , c("pastr")], "past"),
        setNames(luh2v2[, , c("range")], "range"),
        setNames(forestry, "forestry"),
        setNames(luh2v2[, , c("primf")], "primforest"),
        setNames(secondary_forest, "secdforest"),
        setNames(luh2v2[, , c("urban")], "urban"),
        setNames(luh2v2[, , c("primn")], "primother"),
        setNames(luh2v2[, , c("secdn")], "secdother"))

    } else if (fao_corr == TRUE) {
      out <- calcOutput("FAOForestRelocate", selectyears = selectyears, cells = cells, aggregate = FALSE)
      cat("Reallocation in land pools results in extremely low negative values for", unique((where(out < 0)$true$individual)[, 3]), "land in", where(out < 0)$true$regions, "with a range of", range(out[out < 0]))
      cat("\nSuch values are replaced with 0.")
      out[out < 0] <- 0
    }
  }

  return(.out(out, cellular))
}
