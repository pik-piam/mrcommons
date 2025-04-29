#' Calculation of N2O MAC curves of Energy Industry and Landuse
#'
#' Calculation of the N2O relative costcurves (subtypes: Energy Industry and
#' Landuse) weighted by the baseline emissions. Sources: N2O Transport, N2O
#' Adipic acid production, N2O Nitric acid production, N2O Fertilizer, N2O
#' Animal waste, N2O Domestic sewage divided in classes 1-201.
#'
#'
#' @param sector "all" or "landuse"; "all"" includes energy_industry and landuse
#' @param source "ImageMacc" or "PBL_MACC_2019"
#' @return MAgPIE object
#' @author Nele Steinmetz, Florian Humpenoeder, Michael Windisch
#' @seealso [madrat::calcOutput()], [readImageMacc()],
#' [convertImageMacc()]
#' @examples
#' \dontrun{
#' calcOutput("MACCsN2O")
#' }
#' @importFrom magclass getNames
calcMACCsN2O <- function(sector = "all", source = "ImageMacc") {  # nolint: object_name_linter.

  # readSource N2O and baseline Emissions
  if (source == "ImageMacc") { # nolint

    unit <- "Abatement fraction. Cost dimension is in tax steps of 5 2005USD/t each"
    description <- "N2O ImageMacc"

    energyInd <- readSource("ImageMacc", "N2O_Energy_Industry")
    landuse <- readSource("ImageMacc", "N2O_Landuse")

    n2o <- mbind(energyInd, landuse)
    n2o <- time_interpolate(n2o, seq(2015, 2095, 10), integrate_interpolated_years = TRUE,
                            extrapolation_type = "linear")

    getNames(n2o) <- gsub("N2O Transport", "n2otrans", getNames(n2o))
    getNames(n2o) <- gsub("N2O Adipic acid production", "n2oadac", getNames(n2o))
    getNames(n2o) <- gsub("N2O Nitric acid production", "n2onitac", getNames(n2o))
    getNames(n2o) <- gsub("N2O Fertilizer", "n2ofert", getNames(n2o))
    getNames(n2o) <- gsub("N2O Animal waste", "n2oanwst", getNames(n2o))
    getNames(n2o) <- gsub("N2O Domestic sewage", "n2owaste", getNames(n2o))

    # weight for the aggregation
    baseline <- readSource("ImageMacc", "baseline_sources")
    w <- baseline[, getYears(n2o), c("N2O Transport", "N2O Adipic acid production", "N2O Nitric acid production",
                                     "N2O Fertilizer", "N2O Animal waste", "N2O Domestic sewage")]

    getNames(w) <- gsub("N2O Transport", "n2otrans", getNames(w))
    getNames(w) <- gsub("N2O Adipic acid production", "n2oadac", getNames(w))
    getNames(w) <- gsub("N2O Nitric acid production", "n2onitac", getNames(w))
    getNames(w) <- gsub("N2O Fertilizer", "n2ofert", getNames(w))
    getNames(w) <- gsub("N2O Animal waste", "n2oanwst", getNames(w))
    getNames(w) <- gsub("N2O Domestic sewage", "n2owaste", getNames(w))

  } else if (source == "PBL_MACC_2019") { # nolint
    unit <- "Tax level 200 steps each 20$/tC"
    description <- "N2O PBL_MACC_2019"

    wantedYears <- seq(2010, 2100, by = 5)

    n2o <- NULL
    for (subtype in c("n2otrans", "n2oadac", "n2onitac", "n2ofert", "n2oanwst", "n2owaste")) {
      x <- readSource("PBL_MACC_2019", subtype)
      existingYears <- getYears(x, as.integer = TRUE)
      tmp <- setdiff(wantedYears, existingYears)
      missingYears <- tmp[tmp < existingYears[1]]
      x <- x[, intersect(wantedYears, existingYears), ]
      x <- toolFillYears(x, c(missingYears, getYears(x, as.integer = TRUE)))
      y <- time_interpolate(x, wantedYears, integrate_interpolated_years = TRUE, extrapolation_type = "linear")
      names(dimnames(y)) <- names(dimnames(x))
      n2o <- mbind(n2o, y)
    }

    # weight for the aggregation
    baseline <- readSource("PBL_MACC_2019", "baseline_sources")
    w <- baseline[, getYears(n2o), getNames(n2o, dim = 1)]

  } else if (source == "PBL_MACC_SSP2_2019") { # nolint
    # relative to PBL IMAGE SSP2 BASELINE emissions
    unit <- "Tax level 200 steps each 20$/tC"
    description <- "N2O PBL_MACC_SSP2_2019"

    wantedYears <- seq(2010, 2100, by = 5)

    n2o <- NULL
    for (subtype in c("SSP2_n2otrans", "SSP2_n2oadac", "SSP2_n2onitac", "SSP2_n2ofert",
                      "SSP2_n2oanwst", "SSP2_n2owaste")) {
      x <- readSource("PBL_MACC_2019", subtype)
      existingYears <- getYears(x, as.integer = TRUE)
      tmp <- setdiff(wantedYears, existingYears)
      missingYears <- tmp[tmp < existingYears[1]]
      x <- x[, intersect(wantedYears, existingYears), ]
      x <- toolFillYears(x, c(missingYears, getYears(x, as.integer = TRUE)))
      y <- time_interpolate(x, wantedYears, integrate_interpolated_years = TRUE, extrapolation_type = "linear")
      names(dimnames(y)) <- names(dimnames(x))
      n2o <- mbind(n2o, y)
    }

    # weight for the aggregation
    baseline <- readSource("PBL_MACC_2019", "baseline_sources")
    w <- baseline[, getYears(n2o), getNames(n2o, dim = 1)]

  } else if (source == "PBL_MACC_2022") { # nolint
    unit <- "Tax level 200 steps each 20$/tC"
    description <- "N2O PBL_MACC_2022"

    wantedYears <- seq(2010, 2100, by = 5)

    n2o <- NULL
    for (subtype in c("n2otrans", "n2oadac", "n2onitac", "n2ofert", "n2oanwst", "n2owaste")) {
      for (scentype in c("Default", "Optimistic", "Pessimistic")) {
        x <- readSource("PBL_MACC_2022", subtype, scentype)
        existingYears <- getYears(x, as.integer = TRUE)
        tmp <- setdiff(wantedYears, existingYears)
        missingYears <- tmp[tmp < existingYears[1]]
        x <- x[, intersect(wantedYears, existingYears), ]
        x <- toolFillYears(x, c(missingYears, getYears(x, as.integer = TRUE)))
        y <- time_interpolate(x, wantedYears, integrate_interpolated_years = TRUE, extrapolation_type = "linear")
        names(dimnames(y)) <- names(dimnames(x))
        n2o <- mbind(n2o, y)
      }
    }

    # weight for the aggregation
    baseline <- readSource("PBL_MACC_2019", "baseline_sources")
    w <- baseline[, getYears(n2o), getNames(n2o, dim = 1)]
    w[, , ] <- setYears(w[, 2010, ], NULL)

  } else if (source == "PBL_MACC_SSP2_2022") { # nolint
    unit <- "Tax level 200 steps each 20$/tC"
    description <- "N2O PBL_MACC_SSP2_2022"

    wantedYears <- seq(2010, 2100, by = 5)

    n2o <- NULL
    for (subtype in c("n2otrans", "n2oadac", "n2onitac", "n2ofert", "n2oanwst", "n2owaste")) {
      for (scentype in c("Default", "Opt", "Pess")) {
        x <- readSource("PBL_MACC_SSP2_2022", subtype, scentype)
        existingYears <- getYears(x, as.integer = TRUE)
        tmp <- setdiff(wantedYears, existingYears)
        missingYears <- tmp[tmp < existingYears[1]]
        x <- x[, intersect(wantedYears, existingYears), ]
        x <- toolFillYears(x, c(missingYears, getYears(x, as.integer = TRUE)))
        y <- time_interpolate(x, wantedYears, integrate_interpolated_years = TRUE, extrapolation_type = "linear")
        names(dimnames(y)) <- names(dimnames(x))
        n2o <- mbind(n2o, y)
      }
    }

    # Rename types to match other versions
    getItems(n2o, 3.2)[getItems(n2o, 3.2) == "Pess"] <- "Pessimistic"
    getItems(n2o, 3.2)[getItems(n2o, 3.2) == "Opt"] <- "Optimistic"

    # Some of the original data actually contains abatement levels >1. That shouldnt happen here
    n2o[n2o > 1] <- 1
    n2o[n2o < 0] <- 0

    # weight for the aggregation
    baseline <- readSource("PBL_MACC_SSP2_2022", "IMAGESSP2Baseline")
    w <- baseline[, getYears(n2o), getNames(n2o, dim = 1)]
    w[, , ] <- setYears(w[, 2010, ], NULL)

  }

  # asigning a very small number to countries with zero emissions so if regions that are resulting from
  # zero emission country aggregations still have a value associated
  w[w == 0] <- 1e-10

  if (sector == "all") {
    remYears <- c(seq(2010, 2055, by = 5), seq(2060, 2100, by = 10))
    n2o <- n2o[, remYears, ]
    w <- w[, remYears, ]
  } else if (sector == "landuse") {
    n2o <- n2o[, , c("n2ofert", "n2oanwst")]
    getNames(n2o) <- gsub("n2ofert", "inorg_fert_n2o", getNames(n2o))
    getNames(n2o) <- gsub("n2oanwst", "awms_manure_n2o", getNames(n2o))
    x <- new.magpie(getItems(n2o, dim = 1), seq(2105, 2150, 5), getNames(n2o), 0)
    x[, , ] <- setYears(n2o[, 2100, ], NULL)
    n2o <- mbind(n2o, x)

    w <- w[, , c("n2ofert", "n2oanwst")]
    getNames(w) <- gsub("n2ofert", "inorg_fert_n2o", getNames(w))
    getNames(w) <- gsub("n2oanwst", "awms_manure_n2o", getNames(w))
    x <- new.magpie(getItems(w, dim = 1), seq(2105, 2150, 5), getNames(w), 0)
    x[, , ] <- setYears(w[, 2100, ], NULL)
    w <- mbind(w, x)
  }

  return(list(x = n2o, weight = w, unit = unit, description = description))
}
