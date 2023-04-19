#' @title calcLPJmLClimateInput
#' @description Handle LPJmL climate input data and its time behaviour
#'              (smoothing and harmonizing to baseline)
#'
#' @param climatetype Switch between different climate scenario
#' @param variable Switch between different climate inputs and temporal resolution
#' @param stage Degree of processing: raw, smoothed, harmonized, harmonized2020
#' @param lpjmlVersion LPJmL Version hand over
#'
#' @return magpie object in cellular resolution
#' @author Marcos Alves, Kristine Karstens, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("LPJmLClimateInput",
#'            climatetype = "MRI-ESM2-0:ssp370",
#'            variable = "temperature:annualMean")
#' }
#'
#' @importFrom madrat toolSplitSubtype toolTimeAverage
#' @importFrom magclass getNames
#' @importFrom magpiesets findset
#' @importFrom mstools toolHoldConstant
#' @importFrom SPEI thornthwaite
#'

calcLPJmLClimateInput <- function(climatetype = "MRI-ESM2-0:ssp370",
                                  variable = "temperature:annualMean",
                                  stage = "harmonized2020",
                                  lpjmlVersion = "LPJmL4_for_MAgPIE_44ac93de") {

  # Create settings for LPJmL/GCM from version and climatetype argument
  cfg <- toolClimateInputVersion(lpjmlVersion = lpjmlVersion,
                                 climatetype = climatetype)
  var <- toolSplitSubtype(variable, list(type = NULL, timeres = NULL))
  outtype <- ifelse(var$timeres != "wetDaysMonth", var$type, "wetDaysMonth")

  if (stage %in% c("raw", "smoothed")) {
    ########## PLUG HIST + FUTURE ##########


    if (!grepl("historical", climatetype)) {

      .subtypeScen <- paste(cfg$versionScen, cfg$climatetype, var$type, sep = ":")
      .subtypeHist <- gsub("ssp[0-9]{3}", "historical", .subtypeScen)

      # For climate scenarios historical GCM data has to be read in from a different file
      x <- mbind(readSource("LPJmLClimateInput", subtype = .subtypeHist,
                            subset = var$timeres, convert = "onlycorrect"),
                 readSource("LPJmLClimateInput", subtype = .subtypeScen,
                            subset = var$timeres, convert = "onlycorrect"))
      years <- getYears(x, as.integer = TRUE)
      x     <- x[, years[years >= 1951], ]

    } else {

      .subtypeHist <- paste(cfg$versionHist, cfg$climatetype, var$type, sep = ":")
      x     <- readSource("LPJmLClimateInput", subtype = .subtypeHist,
                          subset = var$timeres, convert = "onlycorrect")
      years <- getYears(x, as.integer = TRUE)
      x     <- x[, years[years >= 1930], ]
    }
    ########## PLUG HIST + FUTURE ##########

    if (stage == "smoothed") {
      out <- toolSmooth(x)
    } else {
      out <- x
    }

  } else if (grepl("harmonized", stage)) {

    harmStyle <- switch(outtype,
                        "temperature"   = "additive",
                        "precipitation" = "limited",
                        "longWaveNet"   = stop(paste0("No harmonization available for: ", var$variable)),
                        "shortWave"     = stop(paste0("No harmonization available for: ", var$variable)),
                        "wetDaysMonth"  = stop(paste0("No harmonization available for: ", var$variable)))

    if (stage == "harmonized") {
      # read in historical data for subtype
      baseline <- calcOutput("LPJmLClimateInput", climatetype = cfg$baselineHist,
                             variable = variable, stage = "smoothed",
                             lpjmlVersion = lpjmlVersion, aggregate = FALSE)
      x        <- calcOutput("LPJmLClimateInput", climatetype = cfg$climatetype,
                             variable = variable, stage = "smoothed",
                             lpjmlVersion = lpjmlVersion, aggregate = FALSE)
      out <- toolHarmonize2Baseline(x, baseline, ref_year = cfg$refYearHist, method = harmStyle)

    } else if (stage == "harmonized2020") {
      # read in historical data for subtype
      baseline2020 <- calcOutput("LPJmLClimateInput", climatetype = cfg$baselineGcm,
                                 variable = variable, stage = "harmonized",
                                 lpjmlVersion = lpjmlVersion, aggregate = FALSE)

      if (cfg$climatetype    == cfg$baselineGcm) {

        out <- baseline2020

      } else {

        x   <- calcOutput("LPJmLClimateInput", climatetype = cfg$climatetype,
                          variable = variable, stage = "smoothed",
                          lpjmlVersion = lpjmlVersion, aggregate = FALSE)
        out <- toolHarmonize2Baseline(x, baseline2020, ref_year = cfg$refYearGcm, method = harmStyle)
      }

    } else {
      stop("Stage argument not supported!")
    }
  } else {
    stop("Stage argument not supported!")
  }

  unit <- switch(outtype,
                 "temperature"   = "Degree Celcius",
                 "precipitation" = "mm/day",
                 "longWaveNet"   = "watt per m2",
                 "shortWave"     = "watt per m2",
                 "wetDaysMonth"  = "number of rainy days")

  description <- switch(outtype,
                        "temperature"   = paste0("Average ", var$timeres, " air temperature"),
                        "precipitation" = paste0("Average ", var$timeres, " precipitation"),
                        "longWaveNet"   = "Long wave radiation",
                        "ShortWave"     = "Short wave radiation",
                        "wetDaysMonth"  = "number of rainy days")

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
