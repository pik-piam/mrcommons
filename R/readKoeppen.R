#' @title readKoeppen
#' @description Read Koeppen climate zones on iso-country and cellular level
#' @param subtype Switch between different levels
#' @return List of magpie objects with results on country or cellular level
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#' readSource("Koeppen", subtype = "iso")
#' }
#'
#' @importFrom utils read.csv

readKoeppen <- function(subtype = "iso") {

  if (subtype == "iso") {

    x         <- read.csv("kgzones.csv")
    x$country <- NULL
    mag       <- as.magpie(x, spatial = 1, temporal = 0, datacol = 2)

  } else if (subtype == "cellular") {   # nolint

    alltimes  <- c("1951-1975_ASCII.txt", "1976-2000_ASCII.txt", "2001-2025_A2_ASCII.txt")
    out       <- NULL

    # configure climate zones from 1951-1975_ASCII.txt
    # (as 2001-2025_A2_ASCII.txt does not include Dwd)
    file      <- alltimes[1]
    x         <- read.table(file, skip = 1, header = TRUE, stringsAsFactors = FALSE, allowEscapes = TRUE)
    zones2ID  <- data.frame(Cls = sort(unique(x$Cls)), ID = seq_along(unique(x$Cls)))

    # Loop over files
    for (i in seq_along(alltimes)) {

      file      <- alltimes[i]
      x         <- read.table(file, skip = 1, header = TRUE, stringsAsFactors = FALSE, allowEscapes = TRUE)
      x         <- merge(x, zones2ID) # use IDs rather than zone names

      grid           <- array(NA, dim = c(720, 360))
      irowicol       <- cbind(2 * (x[, "Lon"] + 180 + 0.25), 2 * (x[, "Lat"] + 90 + 0.25))
      grid[irowicol] <- x[, "ID"]     # use IDs rather than zone names

      map         <- toolGetMappingCoord2Country(pretty = TRUE)
      magpieCoord <- array(c(map$lon, map$lat), dim = c(67420, 2),
                           dimnames = list(paste(map$coords, map$iso, sep = "."),
                                           c("lon", "lat")))
      cellNames <- paste(map$coords, map$iso, sep = ".")
      years     <- as.numeric(unlist(regmatches(file, gregexpr("\\d{4}", file))))
      # only save first year to reduce data
      mag       <- array(0, dim = c(length(cellNames), 1, 1),
                         dimnames = list(cellNames, years[1], "ID"))
      translateGrid2Mag <- t(2 * t(magpieCoord) + c(360, 180) + 0.5)
      mag[]     <- grid[translateGrid2Mag]
      mag       <- as.magpie(mag, spatial = 1)
      getSets(mag)  <- c("x.y.iso", "t", "data")
      mag       <- rast(as.RasterBrick(mag))
      mag       <- terra::focal(mag, w = 9, "modal", na.policy = "only", na.rm = TRUE)
      mag       <- as.magpie(terra::extract(mag, map[c("lon", "lat")])[, -1], spatial = 1)
      getSets(mag)  <- c("x.y.iso", "t", "data")
      getCells(mag) <- cellNames
      getYears(mag) <- years[1]
      out           <- mbind(out, mag)
    }

    mag <- new.magpie(getCells(out), getYears(out), zones2ID$Cls,
                      fill = 0, sets = getSets(out))
    for (i in zones2ID$ID) mag[, , zones2ID$Cls[i]][which(out == zones2ID$ID[i])] <- 1

  } else stop("Invalid subtype! (Valid subtype: 'iso' and 'cellular'")

  return(mag)
}
