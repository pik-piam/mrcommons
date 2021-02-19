#' @title readLPJmL_new
#' @description Read LPJmL content
#' @param subtype Switch between different input
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens, Abhijeet Mishra, Felicitas Beier
#' @seealso
#' \code{\link{readLPJ}}
#' @examples
#'
#' \dontrun{
#' readSource("LPJmL_new", subtype="LPJmL5:CRU4p02.soilc", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @importFrom magpiesets addLocation
#' @importFrom lpjclass readLPJ

readLPJmL_new <- function(subtype="LPJmL5:CRU_4.soilc"){

  split   <- toolSplitSubtype(subtype, list(version=NULL, climatemodel=NULL, scenario=NULL, variable=NULL))$variable
  subtype <- split$variable
  folder  <- paste0(split$version, split$climatemodel, gsub("_", "/", split$scenario), sep="/")
  
  files <- c(soilc              = "soilc_natveg",
             soilc_layer        = "soilc_layer_natveg",
             litc               = "litc_natveg",
             vegc               = "vegc_natveg",
             alitfallc          = "alitfallc_natveg",
             alitterfallc       = "alitterfallc_natveg",
             alitfalln          = "alitfalln_natveg",
             harvest            = "pft_harvest.pft",
             irrig              = "cft_airrig.pft",
             cwater_b           = "cft_consump_water_b.pft",
             sdate              = "sdate",
             hdate              = "hdate",
             mevap_lake         = "mevap_lake",
             input_lake         = "input_lake",
             mtranspiration     = "mtransp_natveg",
             mdischarge         = "mdischarge_natveg",
             mrunoff            = "mrunoff_natveg",
             mevaporation       = "mevap_natveg",
             vegc_grass         = "mean_vegc_mangrass",
             litc_grass         = "litc_mangrass",
             soilc_grass        = "soilc_mangrass"
  )

  file_name   <- toolSubtypeSelect(subtype,files)
  file_name   <- grep(file_name, list.files("."), value=TRUE)
  file_type   <- tail(unlist(strsplit(file_name,'\\.')),1)
  
  if(file_type=="clm"){
   
    filedata <- file(description = file_name, open = "rb", blocking = TRUE,encoding = getOption("encoding"))
    seek(filedata, where=15, origin="start")
    in_header   <- as.numeric(readBin(filedata,what=integer(),size=4,n=5,endian=.Platform$endian))
    start_year  <- in_header[1] 
    nyear       <- in_header[2] 
    nbands      <- in_header[5] 
    years       <- seq(start_year,start_year+nyear,1)
    headlines   <- 51 # generation clm 3
      
  } else {
    
    tmp        <- readLines(file.path(folder,"tmp.out"))
    years      <- as.numeric(unlist(regmatches(tmp, gregexpr("\\d{4}", tmp))))
    start_year <- years[1]
    years      <- seq(years[1],years[2],1)
    headlines  <- 0
    nbands     <- 0
  }
               
  .prepareLPJ <- function(int.nbands   = NULL,
                          int.datatype = numeric(),
                          int.bytes    = 4,
                          int.monthly  = FALSE) {
    
    if(nbands!=0) int.nbands <- nbands 
    
    x <- readLPJ(
      file_name       = file_name,
      wyears          = years,
      syear           = start_year,
      headlines       = headlines,
      averaging_range = 1,
      ncells          = 67420,
      file_type       = "bin",
      bands           = int.nbands,
      datatype        = int.datatype,
      bytes           = int.bytes,
      monthly         = int.monthly
    )
    
    class(x) <- "array"
    x        <- collapseNames(as.magpie(x, spatial=1))
    x        <- addLocation(x)
    
    return(x)
  }
  
  
  if(subtype%in%c("soilc","litc", "vegc", "alitfallc","alitterfallc", "alitfalln",
                  "vegc_grass", "litc_grass", "soilc_grass", "input_lake")){

    x <- .prepareLPJ(int.nbands = 1)
   
  } else if(grepl("*date*", subtype)){

    x <- .prepareLPJ(int.nbands = 24, int.datatype = integer(), int.bytes = 2)
   
  } else if(subtype%in%c("soilc_layer")){

    x <- .prepareLPJ(int.nbands = 5)

  } else if(grepl("transpiration|discharge|runoff|evaporation|evap_lake", subtype)){
    
    x <- .prepareLPJ(int.monthly = TRUE)

  } else if(grepl("harvest|irrig|cwater_b", subtype)){

    x <- .prepareLPJ(int.nbands = 32)
    
  } else {stop(paste0("subtype ",subtype," is not existing"))}

  return(x)
}
