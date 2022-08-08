#' @title readMehta2022
#' @description reads in Global Area Equipped for Irrigation for years 1900-2015 from Mehta et al. (2022)
#'
#' @author  Felicitas Beier
#' @seealso [correctMehta2022()]
#' @examples
#'
#' \dontrun{ a <- readSource("Mehta2022")
#' }

#' @importFrom raster brick raster projectRaster
#' @importFrom terra aggregate project rast
#' @importFrom magclass as.magpie
#' @importFrom magpiesets Cell2Country

readMehta2022 <- function() {

  years  <- c(seq(1900, 1970, by = 10),
              seq(1980, 2015, by = 5))

  files  <- paste0("G_AEI_", years, ".asc")

  .transformObject <- function(x) {

    resolution <- rast(res = 0.5)

    x <- aggregate(x, fact = 6, fun = "sum")
    x <- (project(x, resolution))
    x <- as.magpie(brick(x))

    return(x)
  }

  # read in data and transform to magpie object
  out <- NULL
  for (file in files) {

    aei <- rast(file)
    aei <- .transformObject(x = aei)

    getItems(aei, dim = 2) <- gsub("G_AEI_", "y", getItems(aei, dim = 3))
    getItems(aei, dim = 3) <- "AEI"

    out <- mbind(out, aei)

  }

  # reduce number of cells
  map67420 <- readRDS(system.file("extdata", "mapLPJcells2Coords.rds",
                                  package = "magpiesets"))
  out      <- out[map67420$coords, , ]

  # transform units to Mha
  out <- out * 1e-06

  return(out)
}
