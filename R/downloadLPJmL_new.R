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
#' readSource("LPJmL_new", convert="onlycorrect")
#' }
#'

downloadLPJmL_new <- function(subtype="LPJmL4_for_MAgPIE_3dda0615:GSWP3-W5E5:historical:soilc") {
  
  x     <- toolSplitSubtype(subtype, list(version=NULL, climatemodel=NULL, scenario=NULL, variable=NULL))
  files <- c(soilc              = "soilc_natveg.clm",
             soilc_layer        = "soilc_layer_natveg.clm",
             litc               = "litc_natveg.clm",
             vegc               = "vegc_natveg.clm",
             alitfallc          = "alitfallc_natveg.clm",
             alitterfallc       = "alitterfallc_natveg.clm",
             alitfalln          = "alitfalln_natveg.clm",
             harvest            = "pft_harvest.pft.clm",
             irrig              = "cft_airrig.pft.clm",
             cwater_b           = "cft_consump_water_b.pft.clm",
             sdate              = "sdate.clm",
             hdate              = "hdate.clm",
             mevap_lake         = "mevap_lake.clm",
             input_lake         = "input_lake.clm",
             mtranspiration     = "mtransp_natveg.clm",
             mdischarge         = "mdischarge_natveg.clm",
             mrunoff            = "mrunoff_natveg.clm",
             mevaporation       = "mevap_natveg.clm",
             vegc_grass         = "mean_vegc_mangrass.clm",
             litc_grass         = "litc_mangrass.clm",
             soilc_grass        = "soilc_mangrass.clm"
  )
  
  file_name <- toolSubtypeSelect(x$variable, files)
  file_path <- paste0(x$version, x$climatemodel, gsub("_", "/", x$scenario), file_name, sep = "/")
  storage   <- "/p/projects/cmueller/"
  
  if(file.exists(paste0(storage,file_path))){  
    file.copy(paste0(storage, file_path), file_name)
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
              doi           = meta$doi,
              title         = meta$title,
              author        = meta$authors,
              version       = meta$version,
              release_date  = meta$year,
              description   = meta$description,
              license       = meta$license,
              reference     = meta$citation)
  )
}
