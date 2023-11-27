#' @title       calcMultipleCroppingZones
#' @description This function returns multiple cropping zones at 0.5 degree resolution
#'
#' @param layers 8 for original GAEZ layers,
#'               3 for aggregated multiple cropping zones with
#'               1 = single cropping, 2 = double cropping, 3 = triple cropping
#'               2 for aggregated boolean multicropping potential with
#'               0 = no multicropping (single cropping), 1 = multiple cropping
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE)
#' }
#'
#' @importFrom magclass new.magpie getYears getNames

calcMultipleCroppingZones <- function(layers = 2) {
  # Read in source
  x <- readSource("GAEZv4", subtype = "MCzones", convert = "onlycorrect")

  if (layers == 8) {

    out     <- x

  } else if (layers == 3) {

    mapping <- toolGetMappingCoord2Country(pretty = TRUE)
    out     <- new.magpie(cells_and_regions = paste(mapping$coords, mapping$iso, sep = "."),
                          years = getYears(x),
                          names = getNames(x),
                          sets = getSets(x),
                          fill = NA)
    # Aggregation of multiple cropping zone categories
    out[x == 0] <- 1 # where no data given single-cropping potential assumed
    out[x == 1] <- 1 # where no cropping takes place yet single-cropping potential assumed
    out[x == 2] <- 1 # single cropping -> single cropping
    out[x == 3] <- 1 # limited double cropping -> single cropping
    out[x == 4] <- 2 # double cropping -> double cropping
    out[x == 5] <- 2 # double cropping with rice -> double cropping
    out[x == 6] <- 2 # double rice cropping -> double cropping
    out[x == 7] <- 3 # triple cropping -> triple cropping
    out[x == 8] <- 3 # triple rice cropping -> triple cropping

  } else if (layers == 2) {

    mapping <- toolGetMappingCoord2Country(pretty = TRUE)
    out     <- new.magpie(cells_and_regions = paste(mapping$coords, mapping$iso, sep = "."),
                          years = getYears(x),
                          names = getNames(x),
                          sets = getSets(x),
                          fill = NA)
    # Aggregation of multiple cropping zone categories
    out[x == 0] <- 0 # where no data given single-cropping potential assumed
    out[x == 1] <- 0 # where no cropping takes place yet single-cropping potential assumed
    out[x == 2] <- 0 # single cropping -> single cropping
    out[x == 3] <- 0 # limited double cropping -> single cropping
    out[x == 4] <- 1 # double cropping -> double cropping
    out[x == 5] <- 1 # double cropping with rice -> double cropping
    out[x == 6] <- 1 # double rice cropping -> double cropping
    out[x == 7] <- 1 # triple cropping -> triple cropping
    out[x == 8] <- 1 # triple rice cropping -> triple cropping

  } else {
    stop("Selected number of layers is not available.
         Please select 8 for original GAEZ layers or 3 for reduced layers
         or 2 for boolean whether multiple cropping is possible")
  }

  # Checks
  if (any(is.na(out))) {
    stop("produced NA multiple cropping zones")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "1",
              description  = "multiple cropping zones",
              isocountries = FALSE))
}
