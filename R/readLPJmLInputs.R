#' @title       readLPJmLInputs
#' @description This function reads in LPJmL inputs (inputs to LPJmL)
#' 
#' @param subtype Switch between different inputs
#' 
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' 
#' @author Felicitas Beier
#' 
#' @examples
#' 
#' \dontrun{ 
#' readSource("LPJmLInputs", subtype="lakeshare")
#' }
#'
#' @importFrom magclass read.magpie
#' @importFrom lpjclass readLPJ
#' @importFrom lpjclass read.LPJ_input 
#' @importFrom lpjclass as.lpj 
#' @importFrom utils tail data

readLPJmLInputs <- function(subtype="lakeshare") {
  
  files <- c(cellarea   = "grid.bin",
             oceanshare = "oceanfrac.bin",
             lakeshare  = "glwd_lakes_and_rivers.bin")
  
  file <- toolSubtypeSelect(subtype, files)
  
  # x <- readLPJ(
  #   file_name       = file_name,
  #   wyears          = years,
  #   syear           = start_year,
  #   headlines       = headlines,
  #   averaging_range = 1,
  #   ncells          = 67420,
  #   file_type       = "bin",
  #   bands           = nbands,
  #   datatype        = datatype,
  #   bytes           = bytes,
  #   monthly         = monthly
  # )
  # 
  # class(x) <- "array"
  # x        <- collapseNames(as.magpie(x, spatial=1))
  # x        <- addLocation(x)
  # 
  # 
  # 
  # if (subtype%in%c("oceanshare","lakeshare")){
  #     
  #     unit_transform <- 10^-2
  #     ncells         <- 67420
  #     
  #     if(subtype=="oceanshare") {
  #       header_bytes <- 43
  #     } else if(subtype=="lakeshare"){
  #       header_bytes <- 48
  #     }
  #     
  #     
  #     ### Read input
  #     zz    <- file(paste(file,sep=""),"rb")
  #     seek(zz, where = header_bytes, origin="start")
  #     x     <- array(readBin(con=zz, what=integer(), n=ncells, size=1), dim=c(67420,1,1,1))
  #     
  #     ### Name spatial dimension
  #     lpjclassdata <- NULL
  #     data("lpjclassdata", envir = environment(), package="lpjclass")
  #     land <- lpjclassdata$cellbelongings[,c("LPJ.Index","country.code")]
  #     x <- x[which(lpjclassdata$grid_67420_59199==1),,,,drop=FALSE]
  #     dimnames(x)[[1]] <- paste(land$countryname,1:59199,sep=".")
  #     
  #     ### Transform to magpie object
  #     
  #     x           <- as.magpie(as.lpj(x))
  #     
  #     getNames(x) <- subtype
  #     x           <- x * unit_transform
  #     
  #   } else {
  #     stop(paste0("subtype ",subtype," does not exist"))
  #   } 

  # class(x) <- "array"
  # x        <- collapseNames(as.magpie(x, spatial=1))
  # x        <- addLocation(x)
  
  # return(x)
}  