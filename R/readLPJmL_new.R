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

  subtype     <- toolSplitSubtype(subtype, list(version=NULL, climatemodel=NULL, scenario=NULL, variable=NULL))$variable
  
  .prepareLPJ <- function(datatype = numeric(),
                          bytes    = 4,
                          monthly  = FALSE) {
    
    file_name   <- Sys.glob("*.clm")
    filedata    <- file(description = file_name, open = "rb", blocking = TRUE,encoding = getOption("encoding"))
    seek(filedata, where=15, origin="start")
    in_header   <- as.numeric(readBin(filedata,what=integer(),size=4,n=5,endian=.Platform$endian))
    start_year  <- in_header[1] 
    nyear       <- in_header[2] 
    nbands      <- in_header[5] 
    years       <- seq(start_year,start_year+nyear-1,1)
    headlines   <- 51 # generation clm 3
    
    x <- readLPJ(
      file_name       = file_name,
      wyears          = years,
      syear           = start_year,
      headlines       = headlines,
      averaging_range = 1,
      ncells          = 67420,
      file_type       = "bin",
      bands           = nbands,
      datatype        = datatype,
      bytes           = bytes,
      monthly         = monthly
    )
    
    class(x) <- "array"
    x        <- collapseNames(as.magpie(x, spatial=1))
    x        <- addLocation(x)
    
    return(x)
  }
  
  
  if(subtype%in%c("soilc","litc", "vegc", "alitfallc","alitterfallc", "alitfalln",
                  "vegc_grass", "litc_grass", "soilc_grass", "input_lake")){

    x <- .prepareLPJ(nbands = 1)
   
  } else if(grepl("*date*", subtype)){

    x <- .prepareLPJ(nbands = 24, datatype = integer(), bytes = 2)
   
  } else if(subtype%in%c("soilc_layer")){

    x <- .prepareLPJ(nbands = 5)

  } else if(grepl("transpiration|discharge|runoff|evaporation|evap_lake", subtype)){
    
    x <- .prepareLPJ(monthly = TRUE)

  } else if(grepl("harvest|irrig|cwater_b", subtype)){

    x <- .prepareLPJ(nbands = 32)
    
  } else {stop(paste0("subtype ",subtype," is not existing"))}

  return(x)
}
