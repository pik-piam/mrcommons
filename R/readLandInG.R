#' @title readLandInG
#'
#' @description Reads in LandInG data
#'
#' @param subtype Type of LandInG data that should be read:
#' \itemize{
#' \item \code{physicalArea}: Cropland extend/ physical cropping area separated in irrigated and rainfed
#' \item \code{harvestedArea}: Harvested area separated in different crop types
#' }
#'
#' @return magpie object
#'
#' @importFrom magclass as.magpie collapseNames collapseDim getItems getNames getSets
#' @importFrom magpiesets addLocation
#' @importFrom lpjmlkit read_io
#' @importFrom utils read.delim
#'
#' @author Felicitas Beier
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' A <- readSource("LandInG", subtype = "harvestedArea", aggregate = FALSE)
#' }
#'
readLandInG <- function(subtype = "physicalArea") {

  if (subtype == "physicalArea") {

    bands <- c("rainfed", "irrigated")

    # filename for irrigated and rainfed physical area
    physicalAreaName <- paste0("OutputForMAgPIE_2023-10-20/",
                               "cft_cropland_MAgPIE_cft_aggregation_20200417_20200127_madrat_",
                               "multicropping_LUH2v2_disaggregated_30min_1960-2015.bin")
    # unit: ha

    # read in data and transform to MAgPIE object
    x <- as.magpie(read_io(filename = physicalAreaName,
                           band_names = bands,
                           nstep = 1, timestep = 1))
    # add coordinates
    x <- collapseDim(addLocation(x), dim = "N")
    # rename dimensions
    years <- paste0("y", gsub("-12-31", "", getItems(x, dim = "time")))
    getItems(x, dim = "time") <- years
    x          <- clean_magpie(x)
    getSets(x) <- c("x", "y", "iso", "year", "irrigation")

  } else if (subtype == "harvestedArea") {
    # Ordered list of band names
    # Note: This hard-coded list can be removed as soon as output
    #       is provided as json file.
    bands <- c("rainfed tece",
               "rainfed maiz",
               "rainfed trce",
               "rainfed rice_pro",
               "rainfed soybean",
               "rainfed rapeseed",
               "rainfed groundnut",
               "rainfed sunflower",
               "rainfed oilpalm",
               "rainfed puls_pro",
               "rainfed potato",
               "rainfed cassav_sp",
               "rainfed sugr_cane",
               "rainfed sugr_beet",
               "rainfed others",
               "rainfed cottn_pro",
               "rainfed foddr",
               "rainfed pasture",
               "rainfed begr",
               "rainfed betr",
               "irrigated tece",
               "irrigated maiz",
               "irrigated trce",
               "irrigated rice_pro",
               "irrigated soybean",
               "irrigated rapeseed",
               "irrigated groundnut",
               "irrigated sunflower",
               "irrigated oilpalm",
               "irrigated puls_pro",
               "irrigated potato",
               "irrigated cassav_sp",
               "irrigated sugr_cane",
               "irrigated sugr_beet",
               "irrigated others",
               "irrigated cottn_pro",
               "irrigated foddr",
               "irrigated pasture",
               "irrigated begr",
               "irrigated betr")

    # filename
    harvestedAreaName <- paste0("OutputForMAgPIE_2023-10-20/",
                                "cft_MAgPIE_cft_aggregation_20200417_20200127_madrat_",
                                "multicropping_LUH2v2_disaggregated_30min_1960-2015.bin")
    # unit: ha

    # read in data and transform to MAgPIE object
    x <- as.magpie(read_io(filename = harvestedAreaName,
                           band_names = bands,
                           nstep = 1, timestep = 1))
    # add coordinates
    x <- collapseDim(addLocation(x), dim = "N")
    # rename dimensions
    years                     <- paste0("y", gsub("-12-31", "", getItems(x, dim = "time")))
    getItems(x, dim = "time") <- years
    getItems(x, dim = 3, raw = TRUE) <- gsub(" ", ".", getItems(x, dim = 3))
    x          <- clean_magpie(x)
    getSets(x) <- c("x", "y", "iso", "year", "irrigation", "crop")

  }

  return(x)
}
