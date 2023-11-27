#' @title readGAEZv4
#' @description Read in data from the Global Agro-ecological Zones (GAEZ) data set version 4
#' @param subtype Subtype to be read
#' @return MAgPIE object at 0.5 cellular level
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("GAEZv4", convert = "onlycorrect")
#' }
#'
#' @importFrom magclass as.magpie mbind getNames
#' @importFrom raster brick raster projectRaster

readGAEZv4 <- function(subtype = "MCzones") {

  # Transform from 0.08 to 0.5 spatial resolution and convert to magpie object
  .transformObject <- function(x) {
    x <- brick(projectRaster(from = x, to = raster(res = 0.5), method = "ngb", over = TRUE))
    x <- as.magpie(x)
    return(x)
  }

  if (subtype == "MCzones") {
    ### Multiple cropping zones data
    ## Legend
    # 0: 0
    # 1: no cropping
    # 2: single cropping
    # 3: limited double cropping
    # 4: double cropping
    # 5: double cropping with rice
    # 6: double rice cropping
    # 7: triple cropping
    # 8: triple rice cropping

    ### Rainfed
    mcr <- brick(paste(subtype, "mcr_CRUTS32_Hist_0010.tif", sep = "/"))
    mcr <- .transformObject(x = mcr)
    getNames(mcr) <- "rainfed"

    ### Irrigated
    mci <- brick(paste(subtype, "mci_CRUTS32_Hist_0010.tif", sep = "/"))
    mci <- .transformObject(x = mci)
    getNames(mci) <- "irrigated"

    x  <- mbind(mci, mcr)

  } else {
    stop("This GAEZ subtype is not available yet.
         Please select available subtype, e.g. MCzones for multiple cropping zones")
  }

  if (any(is.na(x))) {
    stop("produced NA multiple cropping zones")
  }

  return(x)
}
