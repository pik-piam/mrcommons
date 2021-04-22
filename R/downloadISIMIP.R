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
  
  if (grepl("airww",subtype)) {
    
  x <- toolSplitSubtype(subtype, list(dataset = "airww", 
                                      model   = c("CLM45", "CLM50", "CWatM", "DBH", "H08", "JULES-W1", "LPJmL", "MATSIRO", "MPI-HM", "ORCHIDEE", "ORCHIDEE-DGVM", "PCR-GLOBWB", "WaterGAP2"),
                                      gcm     = c("gfdl-esm2m","hadgem2-es","ipsl-cm5a-lr","miroc5"),
                                      version = c("2a","2b","3a","3b")))
  
  paths <- c(airww = paste0("ISIMIP",x$version,"/OutputData/water_global/",x$model,"/",x$gcm,"/historical/",
                            tolower(x$model),"_",tolower(x$gcm),"_ewembi_picontrol_histsoc_co2_airrww_global_monthly_1861_2005.nc4"))
                            
  path <- toolSubtypeSelect(x$dataset,paths)
  if (file.exists(paste0("/p/projects/isimip/isimip/",path))) {  
    storage <- "/p/projects/isimip/isimip/"
    file.copy(paste0(storage,path), basename(path))
  } else {
    storage <- "https://files.isimip.org/"
    # download the data
    err <- try(suppressWarnings(download.file(paste0(storage,path), 
                                              destfile = basename(path), mode = "wb", 
                                              quiet = FALSE)), silent = TRUE)
    if(class(err)=="try-error") stop("Data for requested subtype \"",subtype,"\" could not be found!")
  }
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

if (grepl("yields",subtype)) {
  
  
  x <- toolSplitSubtype(subtype, list(dataset = "yields",
                                      model   = c("LPJmL", "EPIC-IIASA", "pDSSAT", "CYGMA1p74"),
                                      gcm     = c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll"),
                                      scen    = c("historical", "ssp126", "ssp370", "ssp585"),
                                      co2     = c("default", "2015co2"),
                                      version = c("2a","2b","3a","3b")))
  
  if (x$scen == "historical") {
    years <- "1861_2005"} else {years <- "2015_2100"}
  
  for (crop in c("mai", "ri1", "ri2", "swh", "soy")){
    for (irr in c("firr", "noirr")) {
      paths <- c(yields = paste0(x$model,"/phase",x$version,"/",x$gcm,"/",x$scen, "/", crop, "/",
                                 tolower(x$model),"_",tolower(x$gcm),"_w5e5_", x$scen, "_2015soc_",x$co2, "_yield-", crop, "-",irr, "_global_annual_", years,".nc4"))
      
      path <- toolSubtypeSelect(x$dataset,paths)
      if (file.exists(paste0("/p/projects/macmit/data/GGCMI/AgMIP.output/",path))) {  
        storage <- "/p/projects/macmit/data/GGCMI/AgMIP.output/"
        file.copy(paste0(storage,path), basename(path))
      } else {
        stop("Data for requested subtype \"",subtype,"\" could not be found!")
      }
    }}
}

  }
  
  

