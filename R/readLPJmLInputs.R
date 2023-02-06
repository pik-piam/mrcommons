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
#' \dontrun{
#' readSource("LPJmLInputs", subtype = "lakeshare", convert = FALSE)
#' }
#'
#' @importFrom magclass as.magpie collapseNames
#' @importFrom lpjclass readLPJ
#' @importFrom magpiesets addLocation

readLPJmLInputs <- function(subtype = "lakeshare") {

  files <- c(lakeshare  = "glwd_lakes_and_rivers.bin")
  file  <- toolSubtypeSelect(subtype, files)

  # Data settings
  if (subtype %in% c("lakeshare")) {

       unitTrans       <- 0.01
       ncells          <- 67420
       wyears          <- 1
       syear           <- 1
       avgRange        <- NULL
       filetype       <- "bin"
       bands           <- 1
       datatype        <- integer()
       bytes           <- 1
       monthly         <- FALSE
  }

  # Read in the data
   x <- readLPJ(file_name       = file,
                wyears          = wyears,
                syear           = syear,
                averaging_range = avgRange,
                ncells          = ncells,
                file_type       = filetype,
                bands           = bands,
                datatype        = datatype,
                bytes           = bytes,
                monthly         = monthly)

   # Unit transformation
   x <- x * unitTrans

   # Transform to magpie object and add dimension details
   class(x) <- "array"
   x        <- collapseNames(as.magpie(x, spatial = 1))
   x        <- addLocation(x)
   x        <- collapseDim(x, dim = "N")
   x        <- clean_magpie(x)

   return(x)
}
