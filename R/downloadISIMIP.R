#' Download ISIMIP data 
#' 
#' Downloads the latest data from ISIMIP.
#' 
#' @param subtype Type of ISIMIP data that should be read. 
#' It consists of variable ("airrww"), 
#' model ("cwatm","h08","lpjml","matsiro","mpi-hm","pcr-globwb"),
#' GCM ("ipsl-cm5a-lr","gfdl-esm2m","miroc5","hadgem2-es")  
#' and database version ("2a","2b","3a","3b"), separated by ":"
#' (e.g. "airww:LPJmL:gfdl-esm2m:2b")
#' @author Jan Philipp Dietrich
#' @importFrom utils download.file unzip person
#' @importFrom XML xmlToDataFrame
#' @importFrom madrat toolSplitSubtype metadataGFZ

downloadISIMIP <- function(subtype) {
  
  x <- toolSplitSubtype(subtype, list(dataset = "airww", 
                                      model   = c("LPJmL", "pcr-globwb"),
                                      gcm     = c("gfdl-esm2m","gswp3"),
                                      version = c("2a","2b","3a","3b")))
  
  paths <- c(airww = paste0("ISIMIP",x$version,"/OutputData/water_global/",x$model,"/",x$gcm,"/historical/",
                            tolower(x$model),"_",tolower(x$gcm),"_ewembi_picontrol_histsoc_co2_airrww_global_monthly_1861_2005.nc4"))
                            
  path <- toolSubtypeSelect(x$dataset,paths)
  storage <- ifelse(file.exists(paste0("/p/isimip/isimip/",path)), "/p/isimip/isimip/", "https://files.isimip.org/")
  
  # download the data
  err <- try(suppressWarnings(download.file(paste0(storage,path), 
                                            destfile = basename(path), mode = "wb", 
                                            quiet = FALSE)), silent = TRUE)
  if(class(err)=="try-error") stop("Data for requested subtype \"",subtype,"\" could not be found!")
    
  .getMetadata <- function(dataset, version) {
    out <- list(doi = NULL, version = NULL, title = NULL, description = NULL)
    # Info about DOIs is available here: https://www.isimip.org/outputdata/dois-isimip-data-sets/
    if (dataset %in% "airww" && version == "2a") {
      out$doi         <- "http://doi.org/10.5880/PIK.2019.003"
      out$version     <- "v1.1"
      out$title       <- ""
      out$description <- ""
    }
    return(out)
  }
  meta1 <- .getMetadata(x$dataset, x$version)
  meta2 <- metadataGFZ(meta1$doi)
  

  # Compose meta data
  return(list(url           = paste0(storage,path),
              doi           = meta1$doi,
              title         = meta1$title,
              author        = meta2$authors,
              version       = meta1$version,
              release_date  = meta2$year,
              description   = meta1$description,
              license       = meta2$license,
              reference     = meta2$citation)
  )
}
