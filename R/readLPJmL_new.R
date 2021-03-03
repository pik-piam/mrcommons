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
#' readSource("LPJmL_new", convert=FALSE)
#' }
#'
#' @import madrat
#' @importFrom magpiesets addLocation
#' @importFrom lpjclass readLPJ
#' @importFrom stringr str_subset str_trim str_split

readLPJmL_new <- function(subtype="LPJmL4_for_MAgPIE_84a69edd:GSWP3-W5E5:historical:soilc"){

  subtype     <- toolSplitSubtype(subtype, list(version=NULL, climatemodel=NULL, scenario=NULL, variable=NULL))$variable
  
   .prepareLPJ <- function(datatype = numeric(),
                          bytes    = 4,
                          monthly  = FALSE,
                          nbands   = NULL) { # nbands will be overwritten for clm data
    
    file_name   <- Sys.glob(c("*.bin","*.clm"))
    file_type   <- tail(unlist(strsplit(file_name,'\\.')),1)
    
    if(file_type=="clm"){
      
      filedata    <- file(description = file_name, open = "rb", blocking = TRUE,encoding = getOption("encoding"))
      seek(filedata, where=15, origin="start")
      in_header   <- as.numeric(readBin(filedata,what=integer(),size=4,n=5,endian=.Platform$endian))
      start_year  <- in_header[1] 
      nyear       <- in_header[2] 
      nbands      <- in_header[5]            # nbands will be overwritten for clm data
      years       <- seq(start_year,start_year+nyear-1,1)
      headlines   <- 51                      # generation clm 3
      close(filedata)
      
    } else if(file_type=="bin"){
      
      out        <- readLines("lpjml_log.out")
      start_year <- as.numeric(str_trim(unlist(str_split(str_subset(out,'Output written in year:'),":")))[2])
      end_year   <- as.numeric(str_trim(unlist(str_split(str_subset(out,'Last year:'),":")))[2])
      years      <- seq(start_year,end_year,1)
      headlines  <- 0
      
    } else {stop("File format of LPJmL input data unknown. Please provide .clm or .bin file format.")}

    
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
    x        <- collapseDim(addLocation(x), dim="N")
    x        <- clean_magpie(x)
  
    return(x)
  }
  
  if(subtype%in%c("soilc","litc", "vegc", "alitfallc","alitterfallc", "aet",
                  "vegc_grass", "litc_grass", "soilc_grass", "aprec")){

    x <- .prepareLPJ(nbands = 1)
   
  } else if(grepl("*date*", subtype)){

    x <- .prepareLPJ(nbands = 24, datatype = integer(), bytes = 2)
   
  } else if(subtype%in%c("soilc_layer")){

    x <- .prepareLPJ(nbands = 5)

  } else if(grepl("mdischarge|mrunoff|mpet", subtype)){
    
    x <- .prepareLPJ(monthly = TRUE)

  } else if(grepl("harvest|irrig|cwater_b", subtype)){

    x <- .prepareLPJ(nbands = 32)
    
  } else {stop(paste0("subtype ",subtype," is not existing"))}

  return(x)
}
