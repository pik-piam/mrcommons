#' @title calcLPJmL_new
#' @description Handle LPJmL data and its time behaviour (smoothing and harmonizing to baseline)
#'
#' @param version Switch between LPJmL versions (including addons for further version specification)
#' @param climatetype Switch between different climate scenarios
#' @param subtype Switch between different lpjml input as specified in readLPJmL
#' @param subdata Switch between data dimension subitems
#' @param stage Degree of processing: raw, smoothed - raw or smoothed data from 1930|1951
#'                                    raw1901, smoothed1901 - raw or smoothed data from 1901
#'                                    harmonized, harmonized2020 - based on toolLPJmLVersion
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#'
#' @author Kristine Karstens, Felicitas Beier
#'
#' @importFrom madrat calcOutput readSource toolSubtypeSelect toolSplitSubtype
#' @importFrom magclass dimSums getYears setYears
#'
#' @seealso
#' [readLPJmL()]
#' @examples
#' \dontrun{
#' calcOutput("LPJmL_new", subtype = "soilc", aggregate = FALSE)
#' }
#'
calcLPJmL_new <- function(version = "LPJmL4_for_MAgPIE_44ac93de", # nolint
                          climatetype = "MRI-ESM2-0:ssp370",
                          subtype = "soilc", subdata = NULL, stage = "harmonized2020") {

  # Create settings for LPJmL from version and climatetype argument
  cfg <- toolLPJmLVersion(version = version, climatetype = climatetype)

  if (grepl("raw|smoothed", stage)) {

    if (subtype %in% c("discharge", "runoff", "lake_evap", "input_lake")) {
      # calcLPJmL subtypes (returned by calcLPJmL) that are calculated based on different original LPJmL subtypes
      readinmap  <- c(lake_evap    = "mpet",  # mpet_natveg    lake_evap  = pet   * lake_shr * cell_area
                      input_lake   = "aprec", # aprec_natveg   input_lake = aprec * lake_shr * cell_area
                      discharge    = "mdischarge",
                      runoff       = "mrunoff")

      subtypeIn <- toolSubtypeSelect(subtype, readinmap)

    } else {
      subtypeIn <- subtype
    }

    if (grepl("\\+scen", cfg$readin_version)) {
      scen <- gsub("(.+)\\+scen:([^\\+]*)(.*)", "_\\2", cfg$readin_version)
      cfg$readin_version <- gsub("\\+scen:([^\\+]*)", "", cfg$readin_version)
      cfg$climatetype    <- paste0(cfg$climatetype, scen)
    }
    readinName <- paste(cfg$readin_version, cfg$climatetype, subtypeIn, sep = ":")
    readinHist <- gsub("ssp[0-9]{3}", "historical", readinName)

    ########## PLUG HIST + FUTURE ##########

    if (!grepl("historical", cfg$climatetype)) {

      x     <- mbind(readSource("LPJmL_new", subtype = readinHist, convert = FALSE),
                     readSource("LPJmL_new", subtype = readinName, convert = FALSE))
      years <- getYears(x, as.integer = TRUE)
      x     <- x[, years[years >= 1951], ]

    } else {

      x     <- readSource("LPJmL_new", subtype = readinName, convert = FALSE)
      years <- getYears(x, as.integer = TRUE)
      if (!grepl("1901", stage)) x <- x[, years[years >= 1930], ]

    }
    ########## PLUG HIST + FUTURE ##########

    if (!is.null(subdata)) {
      if (!all(subdata %in% getNames(x))) {
        stop(paste0("Subdata items '", subdata, "' are not part of selected LPJmL subtype!"))
      }
      x <- x[, , subdata]
    }

    ########## UNIT TRANSFORMATION ###############

    if (grepl("soilc|soilc_layer|litc|vegc|alitfallc|alitter|vegc_grass|litc_grass|soilc_grass", subtype)) {

      unitTransform <- 0.01
      x    <- x * unitTransform
      unit <- "tC/ha"

      if (grepl("litc|soilc_layer", subtype)) x <- toolConditionalReplace(x, "<0", 0)

    } else if (grepl("*date*", subtype)) {

      unit <- "day of the year"

    } else if (grepl("aet|cft_transp_pft|discharge|runoff|lake_evap|input_lake", subtype)) {
      # unit transformation
      if (grepl("aet|cft_transp_pft", subtype)) {
        # Annual evapotranspiration (evaporation + transpiration + interception) given in liter/m^2
        # Plant transpiration in liter/m^2 per season
        # Transform units: liter/m^2 -> m^3/ha
        unitTransform <- 10
        x             <- x * unitTransform

      } else if (grepl("discharge", subtype)) {
        # In LPJmL: (monthly) discharge given in hm3/d (= mio. m3/day)
        # Transform units of discharge: mio. m^3/day -> mio. m^3/month
        dayofmonths <- as.magpie(c(jan = 31, feb = 28, mar = 31, apr = 30, may = 31, jun = 30,
                                   jul = 31, aug = 31, sep = 30, oct = 31, nov = 30, dec = 31))
        x           <- x * dayofmonths

        # Annual value (total over all month)
        if (!grepl("^m", subtype)) {
          x <- dimSums(x, dim = 3)
        }

      } else if (grepl("runoff", subtype)) {
        ## In LPJmL: (monthly) runoff given in LPJmL: mm/month
        # Transform units: liter/m^2 -> liter
        # landarea in mio. ha
        landarea <- setYears(collapseNames(dimSums(readSource("LUH2v2", subtype = "states",
                                                              convert = "onlycorrect")[, "y1995", ],
                                                   dim = 3)), NULL)
        x        <- x * landarea * 1e10
        # Transform units: liter -> mio. m^3
        x <- x / (1000 * 1000000)

        # Annual value (total over all month)
        if (!grepl("^m", subtype)) {
          x <- dimSums(x, dim = 3)
        }

      } else if (grepl("lake_evap|input_lake", subtype)) {
        ## In LPJmL: given in mm (=liter/m^2)
        # Multiply by lake share
        lakeShare <- readSource("LPJmLInputs", subtype = "lakeshare", convert = "onlycorrect")
        x          <- x * lakeShare

        # Transform units: liter/m^2 -> liter
        cb        <- toolGetMapping("LPJ_CellBelongingsToCountries.rds",
                                    type = "cell", where = "mrcommons")
        cellArea  <- (111e3 * 0.5) * (111e3 * 0.5) * cos(cb$lat / 180 * pi)
        x         <- x * cellArea

        # Transform units: liter -> mio. m^3
        x <- x / (1000 * 1000000)

        # Annual value (total over all month)
        if (grepl("lake_evap", subtype)) {
          x <- dimSums(x, dim = 3)
        }
      }

      units <- c(aet            = "m^3/ha",
                 cft_transp_pft = "m^3/ha",
                 discharge      = "mio. m^3",
                 mdischarge     = "mio. m^3",
                 lake_evap      = "mio. m^3",
                 input_lake     = "mio. m^3",
                 runoff         = "mio. m^3",
                 mrunoff        = "mio. m^3")

      unit <- toolSubtypeSelect(subtype, units)

    } else if (grepl("*harvest*|gpp_grass", subtype)) {

      yieldTransform <- 0.01 / 0.45
      x    <- x * yieldTransform
      unit <- "tDM/ha"

    } else if (grepl("irrig|cwater_b", subtype)) {
      # Transform units: liter/m^2 (= mm) -> m^3/ha
      irrigTransform  <- 10
      # select only irrigated
      x                <- x[, , "irrigated"] * irrigTransform # units are now: m^3 per ha per year
      unit             <- "m^3/ha"

    } else if (grepl("et_grass", subtype)) {
      # Transform units: liter/m^2 (= mm) -> m^3/ha
      watTransform <- 10
      x            <- x * watTransform
      unit         <- "m^3/ha"

    } else if (grepl("input_lake", subtype)) {

      unit <- "mio. m^3"

    } else if (grepl("cshift", subtype)) {

      unit <- "C/C"

    } else if (grepl("fpc", subtype)) {

      unit <- "ha/ha"

    } else if (grepl("mpet", subtype)) {

      unit <- "mm/month"

    } else {
      stop(paste0("subtype ", subtype, " does not exist"))
    }

    ########## UNIT TRANSFORMATION ###############

    if (grepl("smoothed", stage)) {
      out <- toolSmooth(x)
    } else {
      out <- x
    }

  } else if (stage == "harmonized") {
    # read in historical data for subtype
    baseline        <- calcOutput("LPJmL_new", version = cfg$baseline_version,
                                  climatetype = cfg$baseline_hist, subtype = subtype,
                                  subdata = subdata, stage = "smoothed",
                                  aggregate = FALSE, supplementary = TRUE)

    unit            <- baseline$unit
    baseline        <- baseline$x

    x   <- calcOutput("LPJmL_new", version = cfg$readin_version,
                      climatetype = cfg$climatetype, subtype = subtype,
                      subdata = subdata, stage = "smoothed", aggregate = FALSE)
    out <- toolHarmonize2Baseline(x, baseline, ref_year = cfg$ref_year_hist)

  } else if (stage == "harmonized2020") {
    # read in historical data for subtype
    baseline2020    <- calcOutput("LPJmL_new", version = cfg$baseline_version,
                                  climatetype = cfg$baseline_gcm, subtype = subtype,
                                  subdata = subdata, stage = "harmonized",
                                  aggregate = FALSE, supplementary = TRUE)

    unit            <- baseline2020$unit
    baseline2020    <- baseline2020$x

    if (cfg$climatetype    == cfg$baseline_gcm &&
          cfg$readin_version == cfg$baseline_version) {

      out <- baseline2020

    } else {

      x   <- calcOutput("LPJmL_new", version = cfg$readin_version,
                        climatetype = cfg$climatetype, subtype = subtype,
                        subdata = subdata, stage = "smoothed", aggregate = FALSE)
      out <- toolHarmonize2Baseline(x, baseline2020, ref_year = cfg$ref_year_gcm)
    }

  } else {
    stop("Stage argument not supported!")
  }

  return(list(
              x            = out,
              weight       = NULL,
              unit         = unit,
              min          = 0,
              description  = paste0("Output from LPJmL (", subtype, ") for ",
                                    version, " and ", climatetype,
                                    " at stage: ", stage, "."),
              isocountries = FALSE))
}
