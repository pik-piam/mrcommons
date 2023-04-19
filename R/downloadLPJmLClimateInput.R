#' @title downloadLPJmLClimateInput
#' @description Download GCM climate input used for LPJmL runs
#'
#' @param subtype Switch between different inputs (e.g. "ISIMIP3b:IPSL-CM6A-LR:historical:1850-2014:temperature")
#'                Argument consists of GCM version, climate model, scenario and variable,
#'                separated by ":"
#'
#' @return metadata entry
#' @author Marcos Alves, Kristine Karstens
#' @examples
#' \dontrun{
#' readSource("LPJmLClimateInput", convert = "onlycorrect")
#' }
#'
downloadLPJmLClimateInput <- function(subtype = "ISIMIP3bv2:MRI-ESM2-0:ssp370:temperature") { # nolint

  x <- toolSplitSubtype(subtype, list(version = NULL, climatemodel = NULL,
                                      scenario = NULL, variable = NULL))

  varList  <- c(temperature    = "tas",
                precipitation  = "pr",
                longWaveNet    = "lwnet",
                shortWave      = "rsds",
                temperatureMin = "tasmin",
                temperatureMax = "tasmax")
  shortVar <- toolSubtypeSelect(x$variable, varList)

  if (x$climatemodel == "GSWP3-W5E5") {
    storage <- "/p/projects/lpjml/input/historical/" # nolint: absolute_path_linter.
  } else {
    storage <- "/p/projects/lpjml/input/scenarios"   # nolint: absolute_path_linter.
  }

  path        <- file.path(storage,                     # historical or scenarios
                           x$version,                   # version: ISIMIP3a or b(v2)
                           gsub("_", "/", x$scenario),  # obsclim e.g. ssp119
                           x$climatemodel)              # GCMs or GSWP3-W5E5

  if (!dir.exists(path)) {
    path <- file.path(storage,
                      x$version,
                      gsub("_", "/", x$scenario),
                      gsub("_", "-", x$climatemodel))
  }

  fileList <- list.files(path)
  file     <- grep(paste0(shortVar, "_"),
                   fileList, value = TRUE)
  filePath <- file.path(path, file)

  if (file.exists(filePath)) {
    file.copy(filePath, file)
  } else {
    stop("Data is not available so far!")
  }

  # Compose meta data
  return(list(url           = paste0(storage, filePath),
              doi           = NULL,
              title         = x$version,
              author        = NULL,
              version       = x$version,
              release_date  = NULL,
              description   = NULL,
              license       = NULL,
              reference     = NULL)
  )
}
