#' @title readMehta2022
#' @description reads in Global Area Equipped for Irrigation for years 1900-2015 from Mehta et al. (2022)
#'
#' @author  Felicitas Beier
#' @seealso [correctMehta2022()]
#' @examples
#'
#' \dontrun{ a <- readSource("Mehta2022")
#' }

#' @importFrom raster brick
#' @importFrom terra aggregate project rast
#' @importFrom magclass as.magpie

readMehta2022 <- function() {

  years  <- c(seq(1900, 1970, by = 10),
              seq(1980, 2015, by = 5))
  years1 <- years[years < 2000]
  years2 <- years[years >= 2000]

  files  <- c(paste0("G_AEI_", years1, ".ASC"),
              paste0("G_AEI_", years2, ".asc"))

  .transformObject <- function(x) {

    resolution <- terra::rast(res = 0.5)

    x <- suppressWarnings(terra::aggregate(x, fact = 6, fun = "sum"))
    x <- suppressWarnings(terra::project(x, resolution))
    x <- suppressWarnings(raster::brick(x))
    x <- as.magpie(x)

    return(x)
  }

  # read in data and transform to magpie object
  out <- NULL
  for (file in files) {

    aei <- terra::rast(file)
    aei <- .transformObject(x = aei)

    getItems(aei, dim = 2) <- gsub("G_AEI_", "y", getItems(aei, dim = 3))
    getItems(aei, dim = 3) <- "AEI"

    out <- mbind(out, aei)
  }

  # reduce number of cells
  map67420 <- readRDS(system.file("extdata", "mapLPJcells2Coords.rds",
                                  package = "magpiesets"))
  out      <- out[map67420$coords, , ]

  # rename cells
  map           <- toolGetMappingCoord2Country()
  out           <- out[map$coords, , ]
  getCells(out) <- paste(map$coords, map$iso, sep = ".")
  getSets(out)  <- c("x", "y", "iso", "year", "data")

  # transform units to Mha
  out <- out * 1e-06

  return(out)
}
