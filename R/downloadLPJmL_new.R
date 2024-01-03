#' @title downloadLPJmL_new
#' @description Download LPJmL content by version, climate model and scenario
#'
#' @param subtype Switch between different input
#' It consists of LPJmL version, climate model, scenario and variable.
#' For pasture lpjml runs, the scenario variable is used to navigate the output folder structure
#' (e.g. 'LPJmL4_for_MAgPIE_3dda0615:GSWP3-W5E5:historical:soilc' or
#' "LPJmL5.2_Pasture:IPSL_CM6A_LR:ssp126_co2_limN_00:soilc_past_hist")
#' @return metadata entry
#' @author Kristine Karstens, Marcos Alves, Felicitas Beier
#' @examples
#' \dontrun{
#' readSource("LPJmL_new", convert = FALSE)
#' }
#' @importFrom utils head
#' @importFrom stringr str_detect
#' @importFrom madrat toolSplitSubtype

downloadLPJmL_new <- function(subtype = "LPJmL4_for_MAgPIE_44ac93de:GSWP3-W5E5:historical:soilc") { # nolint

  x     <- toolSplitSubtype(subtype,
                            list(version = NULL,
                                 climatemodel = NULL,
                                 scenario = NULL,
                                 variable = NULL))

  files <- c(soilc              = "soilc_natveg",
             soilc_layer        = "soilc_layer_natveg",
             litc               = "litc_natveg",
             vegc               = "vegc_natveg",
             alitfallc          = "alitfallc_natveg",
             alitterfallc       = "alitterfallc_natveg",
             alitterfallc_wood  = "alitterfallc_wood_natveg",
             alitterburnc       = "alitterburnc_natveg",
             alitterburnc_wood  = "alitterburnc_wood_natveg",
             harvest            = "pft_harvest.pft",
             irrig              = "cft_airrig.pft",
             cwater_b           = "cft_consump_water_b.pft",
             sdate              = "sdate",
             hdate              = "hdate",
             mpet               = "mpet_natveg",
             met_grass_ir       = "met_grass_ir",
             met_grass_rf       = "met_grass_rf",
             cft_et_grass_ir    = "cft_et_grass_ir",
             cft_et_grass_rf    = "cft_et_grass_rf",
             aprec              = "aprec_natveg",
             aet                = "aet_natveg",
             mdischarge         = "mdischarge_natveg",
             mrunoff            = "mrunoff_natveg",
             mgpp_grass_ir      = "mgpp_grass_ir",
             mgpp_grass_rf      = "mgpp_grass_rf",
             cft_gpp_grass_ir   = "cft_gpp_grass_ir",
             cft_gpp_grass_rf   = "cft_gpp_grass_rf",
             vegc_grass         = "mean_vegc_mangrass",
             litc_grass         = "litc_mangrass",
             soilc_grass        = "soilc_mangrass",
             soilc_past_hist    = "soilc_hist",
             soilc_past_scen    = "soilc_scen",
             grass_pft_hist     = "pft_harvest_hist.pft",
             grass_pft_scen     = "pft_harvest_scen.pft",
             cshift_fast        = "cshift_fast_natveg",
             cshift_slow        = "cshift_slow_natveg",
             fpc                = "fpc.clm")

  # handling the separate sources of grass runs
  if (!grepl("Pasture", x$version, ignore.case = TRUE)) {
    storage   <- "/p/projects/landuse/users/cmueller/"  # nolint: absolute_path_linter.
  } else {
    storage   <- "/p/projects/rd3mod/inputdata/sources/LPJmL/"  # nolint: absolute_path_linter.
  }

  path <- paste(x$version, x$climatemodel, x$scenario, sep = "/")
  if (!dir.exists(file.path(storage, path))) {
    path <- paste(x$version, gsub("-", "_", x$climatemodel), x$scenario, sep = "/")
  }

  listFiles <- list.files(paste0(storage, path))
  file      <- grep(toolSubtypeSelect(x$variable, files), listFiles, value = TRUE)
  filePath  <- paste0(storage, path, "/", file)

  .findFile <- function(storage, path, listFiles, file) {
    outputFiles <- grep(".out", listFiles, value = TRUE)
    filesOut    <- file.path(storage, path, outputFiles)
    order       <- order(file.info(filesOut)$ctime, decreasing = TRUE)
    filesOut    <- filesOut[order]
    outputFiles <- outputFiles[order]
    x   <- sapply(filesOut, function(x) list(readLines(x))) # nolint
    out <- sapply(x, function(x) any(stringr::str_detect(x, file))) # nolint
    return(outputFiles[out][1])
  }

  if (file.exists(filePath)) {
    file.copy(filePath, file)
    if (grepl("Pasture", x$version, ignore.case = TRUE)) {
      files2copy <- .findFile(storage, path, listFiles, file)
      file.copy(file.path(storage, path, files2copy), files2copy, overwrite = TRUE)
    } else {
      file.copy(paste0(storage, path, "/lpjml_log.out"), "lpjml_log.out")
    }
  } else {
    stop("Data is not available so far!")
  }

  # Compose meta data
  return(list(url           = paste0(storage, filePath),
              doi           = NULL,
              title         = x$version,
              author        = list(person("Christoph", "Mueller", email = "cmueller@pik-potsdam.de"),
                                   person("Jens",      "Heinke",  email = "heinke@pik-potsdam.de"),
                                   person("Stephen",   "Writh",   email = "wirth@pik-potsdam.de")),
              version       = x$version,
              release_date  = NULL,
              description   = NULL,
              license       = "Creative Commons Attribution-ShareAlike 4.0 International License (CC BY-SA 4.0)",
              reference     = NULL)
  )
}
