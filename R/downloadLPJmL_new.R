#' @title downloadLPJmL_new
#' @description Download LPJmL content by version, climate model and scenario
#' 
#' @param subtype Switch between different input
#' It consists of LPJmL version, climate model, scenario and variable. 
#' For pasture lpjml runs, the scenario variable is used to navigate the output folder structure 
#' (e.g. 'LPJmL4_for_MAgPIE_3dda0615:GSWP3-W5E5:historical:soilc' or "LPJmL5.2_Pasture:IPSL_CM6A_LR:ssp126_co2_limN_00:soilc_past_hist")
#' @return metadata entry
#' @author Kristine Karstens, Marcos Alves
#' @examples
#' 
#' \dontrun{
#' readSource("LPJmL_new", convert=FALSE)
#' }
#' @importFrom utils head
#' @importFrom stringr str_detect

downloadLPJmL_new <- function(subtype="LPJmL4_for_MAgPIE_84a69edd:GSWP3-W5E5:historical:soilc") {
  
  x     <- toolSplitSubtype(subtype, list(version=NULL, climatemodel=NULL, scenario=NULL, variable=NULL))
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
             aprec              = "aprec_natveg", 
             aet                = "aet_natveg",   
             mdischarge         = "mdischarge_natveg",
             mrunoff            = "mrunoff_natveg",
             vegc_grass         = "mean_vegc_mangrass",
             litc_grass         = "litc_mangrass",
             soilc_grass        = "soilc_mangrass",
             soilc_past_hist    = "soilc_hist",
             soilc_past_scen    = "soilc_scen",
             grass_pft_hist     = "pft_harvest_hist.pft",
             grass_pft_scen     = "pft_harvest_scen.pft",
             cshift_fast        = "cshift_fast_natveg",
             cshift_slow        = "cshift_slow_natveg")
  
  # handling the separate sources of grass runs
  if (!grepl("Pasture", x$version, ignore.case = T)){
    storage   <- "/p/projects/landuse/users/cmueller/"
  } else {
    storage   <- "/p/projects/rd3mod/inputdata/sources/LPJmL/"
  }
  
  path        <- paste(x$version, x$climatemodel, gsub("_", "/", x$scenario), sep = "/")
  if(!dir.exists(path)){path <- paste(x$version, gsub("-", "_",x$climatemodel), gsub("_", "/", x$scenario), sep = "/")}
  list_files  <- list.files(paste0(storage,path))
  file        <- grep(toolSubtypeSelect(x$variable, files), list_files, value=TRUE)
  file_path   <- paste0(storage, path, "/", file)
  
  find_file <- function(storage, path, list_files, file) {
    output_files <- grep(".out", list_files, value = TRUE)
    files_out <- file.path(storage, path, output_files)
    x <- sapply(files_out, readLines)
    out <- sapply(x, function(x) any(stringr::str_detect(x, file)))
    return(output_files[out])
  }

  if (file.exists(file_path)) {
    file.copy(file_path, file)
    if (grepl("Pasture", x$version, ignore.case = TRUE)) {
      files2copy <- find_file(storage, path, list_files, file)
      file.copy(file.path(storage,path,files2copy),files2copy, overwrite=T)
    } else {
      file.copy(paste0(storage, path, "/", head(grep(".out", list_files, value = T), n = 1)), "lpjml_log.out")
    }
  } else {
    stop("Data is not available so far!")
  }
  
  .getMetadata <- function(dataset, version) {
    out <- list(doi = NULL, version = NULL, title = NULL, description = NULL)
    return(out)
  }
  meta <- .getMetadata(x$dataset, x$version)
  
  # Compose meta data
  return(list(url           = paste0(storage,file_path),
              doi           = NULL,
              title         = x$version,
              author        = list(person("Christoph", "Mueller", email = "cmueller@pik-potsdam.de"),
                                   person("Jens",      "Heinke", email = "heinke@pik-potsdam.de"),
                                   person("Stephen",   "Writh",  email = "wirth@pik-potsdam.de")),
              version       = x$version,
              release_date  = NULL,
              description   = NULL,
              license       = "Creative Commons Attribution-ShareAlike 4.0 International License (CC BY-SA 4.0)",
              reference     = NULL)
  )
}