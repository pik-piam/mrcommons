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
#' @return magpie object containing output of the toolbox
#'
#' @importFrom magclass as.magpie collapseNames collapseDim getItems getNames getSets
#' @importFrom magpiesets addLocation
#' @importFrom lpjclass readLPJ
#' @importFrom utils read.delim
#'
#' @author David Hoetten, Felicitas Beier
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' A <- readSource("LanduseToolbox", subtype = "harvestedArea", aggregate = FALSE)
#' }
#'
readLandInG <- function(subtype = "physicalArea") {

  if (subtype == "physicalArea") {

    # filename for irrigated and rainfed physical area
    physicalAreaName <- paste0("OutputForMAgPIE_2023-10-20/",
                               "cft_cropland_MAgPIE_cft_aggregation_20200417_20200127_madrat_",
                               "multicropping_LUH2v2_disaggregated_30min_1960-2015.bin")
    # unit: ha

    physicalArea <- readLPJ(
      file_name = physicalAreaName,
      syear = 1950,
      wyears = 1950:(1950 + 67),
      bands = 2,
      headlines = 51,
      flexbands = TRUE,
      averaging_range = 1,
      ncells = 67420,
      cellyear = TRUE
    )

    class(physicalArea) <- "array"                                             # convert to array
    physicalArea        <- collapseNames(as.magpie(physicalArea, spatial = 1)) # convert to magpie
    physicalArea        <- collapseDim(addLocation(physicalArea), dim = "N")   # add coordinates
    getItems(physicalArea, dim = 3) <- c("rainfed", "irrigated")               # name data
    getSets(physicalArea)["d3.1"]   <- "irrigation"                            # name data
    output <- physicalArea

  } else if (subtype == "harvestedArea") {

    # filename
    harvestedAreaName <- paste0("OutputForMAgPIE_2023-10-20/",
                                "cft_MAgPIE_cft_aggregation_20200417_20200127_madrat_",
                                "multicropping_LUH2v2_disaggregated_30min_1960-2015.bin")
    # unit: ha

    # convert to lpjclass object
    harvestedArea <- readLPJ(
      file_name = harvestedAreaName,
      syear = 1950,
      wyears = 1950:(1950 + 67),
      bands = 40,
      headlines = 51,
      flexbands = TRUE,
      averaging_range = 1,
      ncells = 67420,
      cellyear = TRUE
    )

    class(harvestedArea) <- "array"                                              # convert to array
    harvestedArea        <- collapseNames(as.magpie(harvestedArea, spatial = 1)) # convert to magpie
    harvestedArea        <- collapseDim(addLocation(harvestedArea), dim = "N")   # add coordinates
    cftBands <- read.delim("cft_bands.txt", header = FALSE)                      # get band names from file
    getItems(harvestedArea, dim = 3) <- unlist(unname(cftBands))                 # name data according to file
    getNames(harvestedArea)          <- gsub("\\s", ".", getNames(harvestedArea))         # prepare to add subdim
    getSets(harvestedArea)           <- c("x", "y", "iso", "year", "irrigation", "crop")  # add subdim
    output <- harvestedArea

  }

  output <- clean_magpie(output)

  return(output)
}
