#' Calculation of CH4 MAC curves of Energy Industry and Landuse
#'
#' Calculation of the CH4 relative costcurves (subtypes: Energy Industry and
#' Landuse) weighted by the baseline emissions. Sources: CH4 coal
#' losses/leakages, CH4 oil losses/leakages, CH4 natural gas losses/leakages,
#' CH4 Landfills, CH4 Domestic Sewage, CH4 Wetland rice, CH4 Animals, CH4
#' Animal waste divided in classes 1-201.
#'
#'
#' @param sector "all" or "landuse"; "all" includes energy_industry and landuse
#' @param source "ImageMacc" or "PBL_MACC_2019"
#' @return MAgPIE object
#' @author Nele Steinmetz, Florian Humpenoeder, Michael Windisch
#' @seealso [madrat::calcOutput()], [readImageMacc()],
#' [convertImageMacc()]
#' @examples
#' \dontrun{
#' calcOutput("MACCsCH4")
#' }
#' @importFrom magclass getNames
calcMACCsCH4 <- function(sector = "all", source = "ImageMacc" # nolint: object_name_linter.
) {
  # readSource CH4 and baseline Emissions
  if (source == "ImageMacc") { # nolint

    unit <- "Abatement fraction. Cost dimension is in tax steps of 5 2005USD/t each"
    description <- "CH4 ImageMacc"

    energyInd <- readSource("ImageMacc", "CH4_Energy_Industry")
    landuse <- readSource("ImageMacc", "CH4_Landuse")

    ch4 <- mbind(energyInd, landuse)

    ch4 <- time_interpolate(ch4, seq(2015, 2095, 10), integrate_interpolated_years = TRUE,
                            extrapolation_type = "linear")

    getNames(ch4) <- gsub("CH4 coal losses/leakages", "ch4coal", getNames(ch4))
    getNames(ch4) <- gsub("CH4 oil losses/leakages", "ch4oil", getNames(ch4))
    getNames(ch4) <- gsub("CH4 natural gas losses/leakages", "ch4gas", getNames(ch4))
    getNames(ch4) <- gsub("CH4 Landfills", "ch4wstl", getNames(ch4))
    getNames(ch4) <- gsub("CH4 Domestic Sewage", "ch4wsts", getNames(ch4))
    getNames(ch4) <- gsub("CH4 Wetland rice", "ch4rice", getNames(ch4))
    getNames(ch4) <- gsub("CH4 Animals", "ch4animals", getNames(ch4))
    getNames(ch4) <- gsub("CH4 Animal waste", "ch4anmlwst", getNames(ch4))

    # weight for the aggregation
    baseline <- readSource("ImageMacc", "baseline_sources")
    w <- baseline[, getYears(ch4), c(
      "CH4 coal losses/leakages", "CH4 oil losses/leakages",
      "CH4 natural gas losses/leakages", "CH4 Landfills",
      "CH4 Domestic Sewage", "CH4 Wetland rice", "CH4 Animals",
      "CH4 Animal waste"
    )]

    getNames(w) <- gsub("CH4 coal losses/leakages", "ch4coal", getNames(w))
    getNames(w) <- gsub("CH4 oil losses/leakages", "ch4oil", getNames(w))
    getNames(w) <- gsub("CH4 natural gas losses/leakages", "ch4gas", getNames(w))
    getNames(w) <- gsub("CH4 Landfills", "ch4wstl", getNames(w))
    getNames(w) <- gsub("CH4 Domestic Sewage", "ch4wsts", getNames(w))
    getNames(w) <- gsub("CH4 Wetland rice", "ch4rice", getNames(w))
    getNames(w) <- gsub("CH4 Animals", "ch4animals", getNames(w))
    getNames(w) <- gsub("CH4 Animal waste", "ch4anmlwst", getNames(w))

  } else if (source == "PBL_MACC_2019") { # nolint

    unit <- "Tax level 200 steps each 20$/tC"
    description <- "CH4 PBL_MACC_2019"

    wantedYears <- seq(2010, 2100, by = 5)

    ch4 <- NULL
    for (subtype in c("ch4coal", "ch4oil", "ch4gas", "ch4wstl", "ch4wsts", "ch4rice", "ch4animals", "ch4anmlwst")) {
      x <- readSource("PBL_MACC_2019", subtype)
      existingYears <- getYears(x, as.integer = TRUE)
      tmp <- setdiff(wantedYears, existingYears)
      missingYears <- tmp[tmp < existingYears[1]]
      x <- x[, intersect(wantedYears, existingYears), ]
      x <- toolFillYears(x, c(missingYears, getYears(x, as.integer = TRUE)))
      y <- time_interpolate(x, wantedYears, integrate_interpolated_years = TRUE, extrapolation_type = "linear")
      names(dimnames(y)) <- names(dimnames(x))
      ch4 <- mbind(ch4, y)
    }

    # weight for the aggregation
    baseline <- readSource("PBL_MACC_2019", "baseline_sources")
    w <- baseline[, getYears(ch4), getNames(ch4, dim = 1)]

  } else if (source == "PBL_MACC_SSP2_2019") { # nolint
    # relative to PBL IMAGE SSP2 BASELINE emissions
    unit <- "Tax level 200 steps each 20$/tC"
    description <- "CH4 PBL_MACC_SSP2_2019"

    wantedYears <- seq(2010, 2100, by = 5)

    ch4 <- NULL
    for (subtype in c("SSP2_ch4coal", "SSP2_ch4oil", "SSP2_ch4gas", "SSP2_ch4wstl", "SSP2_ch4wsts", "SSP2_ch4rice",
                      "SSP2_ch4animals", "SSP2_ch4anmlwst")) {
      x <- readSource("PBL_MACC_2019", subtype)
      existingYears <- getYears(x, as.integer = TRUE)
      tmp <- setdiff(wantedYears, existingYears)
      missingYears <- tmp[tmp < existingYears[1]]
      x <- x[, intersect(wantedYears, existingYears), ]
      x <- toolFillYears(x, c(missingYears, getYears(x, as.integer = TRUE)))
      y <- time_interpolate(x, wantedYears, integrate_interpolated_years = TRUE, extrapolation_type = "linear")
      names(dimnames(y)) <- names(dimnames(x))
      ch4 <- mbind(ch4, y)
    }

    # weight for the aggregation
    baseline <- readSource("PBL_MACC_2019", "baseline_sources")
    w <- baseline[, getYears(ch4), getNames(ch4, dim = 1)]

  } else if (source == "PBL_MACC_2022") { # nolint

    unit <- "Tax level 200 steps each 20$/tC"
    description <- "CH4 PBL_MACC_2022"

    wantedYears <- seq(2010, 2100, by = 5)

    ch4 <- NULL
    for (subtype in c("ch4coal", "ch4oil", "ch4gas", "ch4wstl", "ch4wsts", "ch4rice", "ch4animals", "ch4anmlwst")) {
      for (scentype in c("Default", "Optimistic", "Pessimistic")) {
        x <- readSource("PBL_MACC_2022", subtype, scentype)
        existingYears <- getYears(x, as.integer = TRUE)
        tmp <- setdiff(wantedYears, existingYears)
        missingYears <- tmp[tmp < existingYears[1]]
        x <- x[, intersect(wantedYears, existingYears), ]
        x <- toolFillYears(x, c(missingYears, getYears(x, as.integer = TRUE)))
        y <- time_interpolate(x, wantedYears, integrate_interpolated_years = TRUE, extrapolation_type = "linear")
        names(dimnames(y)) <- names(dimnames(x))
        ch4 <- mbind(ch4, y)

      }
    }


    # weight for the aggregation
    baseline <- readSource("PBL_MACC_2019", "baseline_sources")
    w <- baseline[, getYears(ch4), getNames(ch4, dim = 1)]
    w[, , ] <- setYears(w[, 2010, ], NULL)


  } else if (source == "PBL_MACC_SSP2_2022") { # nolint

    unit <- "Tax level 200 steps each 20$/tC"
    description <- "CH4 PBL_MACC_SSP2_2022"

    wantedYears <- seq(2010, 2100, by = 5)

    ch4 <- NULL
    for (subtype in c("ch4coal", "ch4oil", "ch4gas", "ch4wstl", "ch4wsts", "ch4rice", "ch4animals", "ch4anmlwst")) {
      for (scentype in c("Default", "Opt", "Pess")) {
        x <- readSource("PBL_MACC_SSP2_2022", subtype, scentype)
        existingYears <- getYears(x, as.integer = TRUE)
        tmp <- setdiff(wantedYears, existingYears)
        missingYears <- tmp[tmp < existingYears[1]]
        x <- x[, intersect(wantedYears, existingYears), ]
        x <- toolFillYears(x, c(missingYears, getYears(x, as.integer = TRUE)))
        y <- time_interpolate(x, wantedYears, integrate_interpolated_years = TRUE, extrapolation_type = "linear")
        names(dimnames(y)) <- names(dimnames(x))
        ch4 <- mbind(ch4, y)

      }
    }

    # Rename types to match other versions
    getItems(ch4, 3.2)[getItems(ch4, 3.2) == "Pess"] <- "Pessimistic"
    getItems(ch4, 3.2)[getItems(ch4, 3.2) == "Opt"] <- "Optimistic"

    # Some of the original data actually contains abatement levels >1. That shouldnt happen here
    ch4[ch4 > 1] <- 1
    ch4[ch4 < 0] <- 0

    # weight for the aggregation
    baseline <- readSource("PBL_MACC_SSP2_2022", "IMAGESSP2Baseline")
    w <- baseline[, getYears(ch4), getNames(ch4, dim = 1)]
    w[, , ] <- setYears(w[, 2010, ], NULL)


  }


  # asigning a very small number to countries with zero emissions so if regions that are resulting from
  # zero emission country aggergations still have a value associated
  w[w == 0] <- 1e-10

  if (sector == "all") {
    remYears <- c(seq(2010, 2055, by = 5), seq(2060, 2100, by = 10))
    ch4 <- ch4[, remYears, ]
    w <- w[, remYears, ]
  } else if (sector == "landuse") {
    ch4 <- ch4[, , c("ch4rice", "ch4animals", "ch4anmlwst")]
    getNames(ch4) <- gsub("ch4rice", "rice_ch4", getNames(ch4))
    getNames(ch4) <- gsub("ch4animals", "ent_ferm_ch4", getNames(ch4))
    getNames(ch4) <- gsub("ch4anmlwst", "awms_ch4", getNames(ch4))
    x <- new.magpie(getItems(ch4, dim = 1), seq(2105, 2150, 5), getNames(ch4), 0)
    x[, , ] <- setYears(ch4[, 2100, ], NULL)
    ch4 <- mbind(ch4, x)

    w <- w[, , c("ch4rice", "ch4animals", "ch4anmlwst")]
    getNames(w) <- gsub("ch4rice", "rice_ch4", getNames(w))
    getNames(w) <- gsub("ch4animals", "ent_ferm_ch4", getNames(w))
    getNames(w) <- gsub("ch4anmlwst", "awms_ch4", getNames(w))
    x <- new.magpie(getItems(w, dim = 1), seq(2105, 2150, 5), getNames(w), 0)
    x[, , ] <- setYears(w[, 2100, ], NULL)
    w <- mbind(w, x)
  }

  return(list(x = ch4, weight = w, unit = unit, description = description))
}
