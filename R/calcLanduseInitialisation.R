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
#' @param input_magpie applies area fix (set cell with zero area to minimal value to not distrube aggregating to clusters)
#' @param country_level Whether output shall be returned at country level, requires cellular = TRUE
#' @param selectyears default on "past"
#' @return List of magpie object with results on country or cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens, Felcitas Beier, Patrick v. Jeetze
#' @examples
#' \dontrun{
#' calcOutput("LanduseInitialisation")
#' }
#' @importFrom magclass setNames where


calcLanduseInitialisation <- function(cellular = FALSE, nclasses = "seven", fao_corr = TRUE, cells = "magpiecell", country_level = FALSE, selectyears = "past", input_magpie = FALSE) {
  selectyears <- sort(findset(selectyears, noset = "original"))

  if (cellular == FALSE) {
    LUH2v2 <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE, selectyears = selectyears, cellular = FALSE, aggregate = FALSE)

    if (fao_corr == FALSE) {

      # divide secondary forest into forestry and secdf.
      forestry_country <- readSource("FAO_FRA2015", "fac")[, , "PlantFor"]
      forestry_country <- time_interpolate(forestry_country, interpolated_year = selectyears, integrate_interpolated_years = TRUE, extrapolation_type = "constant")
      vcat(verbosity = 3, "Forestry is interpolated for missing years and held constant for the period before FAO starts")

      secondary_forest <- LUH2v2[, , "secdf"] - setNames(forestry_country[, getYears(LUH2v2), ], NULL)
      forestry_overflow <- secondary_forest
      forestry_overflow[forestry_overflow > 0] <- 0
      tmp <- dimSums(forestry_overflow, dim = 1)
      if (any(tmp < 0)) {
        vcat(verbosity = 2, paste("Mismatch of FAO forestry and Hurtt secondary forest:", paste(paste(getYears(tmp), round(tmp, 0), "Mha, "), collapse = " "), ". cut off."))
      }
      secondary_forest[secondary_forest < 0] <- 0
      forestry <- LUH2v2[, , "secdf"] - secondary_forest

      # calculate other landpools
      crop <- dimSums(LUH2v2[, , c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")], dim = 3)


      if (nclasses %in% c("seven", "six")) {
        pasture <- dimSums(LUH2v2[, , c("pastr", "range")], dim = 3)
        other <- dimSums(LUH2v2[, , c("primn", "secdn")], dim = 3)
      }

      if (nclasses == "nine") {
        out <- mbind(
          setNames(crop, "crop"),
          setNames(LUH2v2[, , c("pastr")], "past"),
          setNames(LUH2v2[, , c("range")], "range"),
          setNames(forestry, "forestry"),
          setNames(LUH2v2[, , c("primf")], "primforest"),
          setNames(secondary_forest, "secdforest"),
          setNames(LUH2v2[, , c("urban")], "urban"),
          setNames(LUH2v2[, , c("primn")], "primother"),
          setNames(LUH2v2[, , c("secdn")], "secdother")
        )
      } else if (nclasses == "seven") {
        out <- mbind(
          setNames(crop, "crop"),
          setNames(pasture, "past"),
          setNames(forestry, "forestry"),
          setNames(LUH2v2[, , c("primf")], "primforest"),
          setNames(secondary_forest, "secdforest"),
          setNames(LUH2v2[, , c("urban")], "urban"),
          setNames(other, "other")
        )
      } else if (nclasses == "six") {
        out <- mbind(
          setNames(crop, "crop"),
          setNames(pasture, "past"),
          setNames(forestry, "forestry"),
          setNames(LUH2v2[, , c("primf")] + secondary_forest, "forest"),
          setNames(LUH2v2[, , c("urban")], "urban"),
          setNames(other, "other")
        )
      }
    } else if (fao_corr == TRUE) {
      FAOforest <- calcOutput("ForestArea", selectyears = selectyears, aggregate = FALSE)

      other <- dimSums(LUH2v2[, , c("primn", "secdn")], dim = 3)
      forest <- dimSums(LUH2v2[, , c("primf", "secdf")], dim = 3)

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
      crop <- dimSums(LUH2v2[, , c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")], dim = 3)
      if (nclasses %in% c("seven", "six")) {
        pasture <- dimSums(LUH2v2[, , c("pastr", "range")], dim = 3)
      }

      if (nclasses == "nine") {
        # differentiate corrected other land into primary and secondary
        totother_luh <- dimSums(LUH2v2[, , c("primn", "secdn")], dim = 3)
        primother_shr <- LUH2v2[, , "primn"] / setNames(totother_luh, NULL)
        primother_shr[is.na(primother_shr)] <- 0
        secdother_shr <- LUH2v2[, , "secdn"] / setNames(totother_luh, NULL)
        secdother_shr[is.na(secdother_shr)] <- 0
        primother <- primother_shr * setNames(FAOother, NULL)
        secdother <- secdother_shr * setNames(FAOother, NULL)

        out <- mbind(
          setNames(crop, "crop"),
          setNames(LUH2v2[, , c("pastr")], "past"),
          setNames(LUH2v2[, , c("range")], "range"),
          setNames(LUH2v2[, , c("urban")], "urban"),
          FAOforest[, , c("primforest", "secdforest", "forestry")],
          setNames(primother, "primother"),
          setNames(secdother, "secdother")
        )
      } else if (nclasses == "seven") {
        out <- mbind(
          setNames(crop, "crop"),
          setNames(pasture, "past"),
          setNames(LUH2v2[, , c("urban")], "urban"),
          FAOforest[, , c("primforest", "secdforest", "forestry")],
          setNames(FAOother, "other")
        )
      } else if (nclasses == "six") {
        forest <- dimSums(FAOforest[, , c("primforest", "secdforest")], dim = 3)
        out <- mbind(
          setNames(crop, "crop"),
          setNames(pasture, "past"),
          setNames(LUH2v2[, , c("urban")], "urban"),
          FAOforest[, , c("forestry")],
          setNames(forest, "forest"),
          setNames(FAOother, "other")
        )
      }
    }
  } else {
    if (fao_corr == FALSE) {

      # Load cellular and country data
      countrydata <- calcOutput("LanduseInitialisation", aggregate = FALSE, nclasses = "seven", fao_corr = FALSE, selectyears = selectyears, cellular = FALSE)
      LUH2v2 <- calcOutput("LUH2v2", aggregate = FALSE, cells = cells, landuse_types = "LUH2v2", irrigation = FALSE, cellular = TRUE, selectyears = selectyears)
      LUH2v2 <- toolCell2isoCell(LUH2v2, cells = cells)

      if (cells == "lpjcell") {
        mapping   <- toolGetMappingCoord2Country()
        mapping   <- data.frame(mapping, "celliso" = paste(mapping$iso, 1:67420, sep = "."), stringsAsFactors = FALSE)
        countries <- unique(mapping$iso)
      } else {
        mapping   <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")
        countries <- unique(mapping$iso)
      }
      if (is.null(countries)) stop("There must be something wrong with CountryToCellMapping.csv! No country information found!")

      # divide secondary forest into forestry and secdf.
      forestry_shr <- countrydata[, , "forestry"] / dimSums(countrydata[, , c("forestry", "secdforest")], dim = 3)
      forestry_shr[is.nan(forestry_shr)] <- 0
      forestry_shr <- toolAggregate(x = forestry_shr[countries, , ], rel = mapping, from = "iso", to = "celliso", dim = 1)
      forestry <- forestry_shr * LUH2v2[, , "secdf"]
      secondary_forest <- (1 - forestry_shr) * LUH2v2[, , "secdf"]

      # calculate other landpools
      crop <- dimSums(LUH2v2[, , c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")], dim = 3)
      if (nclasses %in% c("seven", "six")) {
        pasture <- dimSums(LUH2v2[, , c("pastr", "range")], dim = 3)
        other <- dimSums(LUH2v2[, , c("primn", "secdn")], dim = 3)
      }

      if (nclasses == "nine") {
        out <- mbind(
          setNames(crop, "crop"),
          setNames(LUH2v2[, , c("pastr")], "past"),
          setNames(LUH2v2[, , c("range")], "range"),
          setNames(forestry, "forestry"),
          setNames(LUH2v2[, , c("primf")], "primforest"),
          setNames(secondary_forest, "secdforest"),
          setNames(LUH2v2[, , c("urban")], "urban"),
          setNames(LUH2v2[, , c("primn")], "primother"),
          setNames(LUH2v2[, , c("secdn")], "secdother")
        )
      } else if (nclasses == "seven") {
        out <- mbind(
          setNames(crop, "crop"),
          setNames(pasture, "past"),
          setNames(forestry, "forestry"),
          setNames(LUH2v2[, , c("primf")], "primforest"),
          setNames(secondary_forest, "secdforest"),
          setNames(LUH2v2[, , c("urban")], "urban"),
          setNames(other, "other")
        )
      } else if (nclasses == "six") {
        out <- mbind(
          setNames(crop, "crop"),
          setNames(pasture, "past"),
          setNames(forestry, "forestry"),
          setNames(LUH2v2[, , c("primf")] + secondary_forest, "forest"),
          setNames(LUH2v2[, , c("urban")], "urban"),
          setNames(other, "other")
        )
      }
    } else if (fao_corr == TRUE) {
      out <- calcOutput("FAOForestRelocate", nclasses = nclasses, selectyears = selectyears, cells = cells, aggregate = FALSE)
      cat("Reallocation in land pools results in extremely low negative values for", unique((where(out < 0)$true$individual)[, 3]), "land in", where(out < 0)$true$regions, "with a range of", range(out[out < 0]))
      cat("\nSuch values are replaced with 0.")
      out[out < 0] <- 0
    }
  }

  if (input_magpie) {
    if (nclasses %in% c("six", "seven")) {
      other <- "other"
    } else if (nclasses == "nine") {
      other <- "secdother"
    }
    out <- round(out, 8)
    cellArea <- dimSums(out, dim = 3)
    temp <- out[, , other]
    zero <- which(cellArea == 0, arr.ind = F)
    temp[zero] <- 10^-8
    out[, , other] <- temp
  }

  if (country_level) {
    if (cells == "lpjcell") {
      getCells(out) <- toolGetMappingCoord2Country()[, "coords"]
      out <- toolAggregateCell2Country(out, fill = 0)
    } else {
      mapping   <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")
      out <- toolAggregate(out, mapping, from = "celliso", to = "iso")
      out <- toolCountryFill(out, fill = 0)
    }
  }
  
  # Correct locations where there is no land reported
  position <- where(dimSums(out, dim = 3) == 0)$true
  out[position$regions, position$years, "other"] <- 1e-6

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    min = 0,
    max = 14900, ### global land area
    description = "Land use initialisation data for different land pools",
    isocountries = !cellular
  ))
}
