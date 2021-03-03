#' @title downloadLPJmL_new
#' @description Download LPJmL content by version, climate model and scenario
#' 
#' @param subtype Switch between different input
#' It consists of LPJmL version, climate model, scenario and variable 
#' (e.g. 'LPJmL4_for_MAgPIE_3dda0615:GSWP3-W5E5:historical:soilc')
#' @return metadata entry
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{
#' readSource("LPJmL_new", convert=FALSE)
#' }
#'

downloadLPJmL_new <- function(subtype="LPJmL4_for_MAgPIE_84a69edd:GSWP3-W5E5:historical:soilc") {
  
  x     <- toolSplitSubtype(subtype, list(version=NULL, climatemodel=NULL, scenario=NULL, variable=NULL))
  files <- c(soilc              = "soilc_natveg",
             soilc_layer        = "soilc_layer_natveg",
             litc               = "litc_natveg",
             vegc               = "vegc_natveg",
             alitfallc          = "alitfallc_natveg",
             alitterfallc       = "alitterfallc_natveg",
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
             soilc_grass        = "soilc_mangrass"
  )
  
  storage   <- "/p/projects/landuse/users/cmueller/"
  path      <- paste(x$version, x$climatemodel, gsub("_", "/", x$scenario), sep = "/")
  file      <- grep(toolSubtypeSelect(x$variable, files), list.files(paste0(storage,path)), value=TRUE)
  file_path <- paste0(storage, path, "/", file)
  
  if(file.exists(file_path)){  
    file.copy(file_path, file)
    file.copy(paste0(storage, path, "/lpjml_log.out"), "lpjml_log.out")
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
                                   person("Jens",      "Heinke", email = "heinke@pik-potsdam.de")),
              version       = x$version,
              release_date  = NULL,
              description   = NULL,
              license       = "Creative Commons Attribution-ShareAlike 4.0 International License (CC BY-SA 4.0)",
              reference     = NULL)
  )
}
