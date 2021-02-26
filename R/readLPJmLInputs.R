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
#' @importFrom magclass as.magpie collapseNames
#' @importFrom lpjclass readLPJ
#' @importFrom magpiesets addLocation

readLPJmLInputs <- function(subtype="lakeshare") {
  
  files <- c(lakeshare  = "glwd_lakes_and_rivers.bin")
  file  <- toolSubtypeSelect(subtype, files)
  
  # Data settings
  if (subtype%in%c("lakeshare")) {
  
       unit_transform <- 0.01
       ncells         <- 67420
       wyears          = NULL
       syear           = NULL
       averaging_range = NULL
       file_type       = "bin"
       bands           = 67420
       datatype        = integer()
       bytes           = 1
       monthly         = FALSE
  
  }

  # Read in the data
   x <- readLPJ(
     file_name       = file,
     wyears          = wyears,
     syear           = syear,
     averaging_range = averaging_range,
     ncells          = ncells,
     file_type       = file_type,
     bands           = bands,
     datatype        = datatype,
     bytes           = bytes,
     monthly         = monthly
   )
   
   # Unit transformation
   x <- x * unit_transform
   
   # Transform to magpie object and add dimension details
   class(x) <- "array"
   x        <- collapseNames(as.magpie(x, spatial=1))
   x        <- addLocation(x)
   
   return(x)
}  