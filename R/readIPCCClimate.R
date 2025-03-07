#' @title readIPCCClimate
#' @description Read IPCC climate classification
#'
#' @return Magpie object with results on cellular level for 12 IPCC climate zone types
#' @author  Kristine Karstens
#' @examples
#' \dontrun{
#' readSource("IPCCClimate", convert = "onlycorrect")
#' }

readIPCCClimate <-  function() {

  raster1d12 <- rast("CLIMATE_ZONE.rst")
  raster1d12 <- terra::ifel(raster1d12 == 0, NA, raster1d12)
  zoneNames  <- as.character(raster::levels(raster1d12)[[1]]$category)
  raster1d2  <- terra::aggregate(raster1d12, fact = 6, fun = "modal", na.rm = TRUE)

  # fill NAs iterativly
  for (i in 1:4) raster1d2  <- terra::focal(raster1d2, w = 9, "modal", na.policy = "only", na.rm = TRUE)

  # Load celliso names for 1:67420 magpie cells
  map   <- toolGetMappingCoord2Country(pretty = TRUE)
  # Change longitude and latitude
  mag   <- as.magpie(as.numeric(terra::extract(raster1d2, map[c("lon", "lat")])[, -1]), spatial = 1)

  getNames(mag) <- "unknown"
  getYears(mag) <- NULL
  getCells(mag) <- paste(map$coords, map$iso, sep = ".")
  getSets(mag)  <- c("x.y.iso", "t", "data")

  # Tranform 1d-array into matrix over all climate zones
  out   <- add_columns(mag, dim = 3.1, addnm = zoneNames)
  out[] <- 0
  for (zone in c(zoneNames)) {
    out[, , zone][which(mag == which(zoneNames == zone))] <- 1
  }

  # Leaving all cells with unknown climate zone in 'unknown'
  out[, , "unknown"][is.na(mag)] <- 1
  out <- mbind(out[, , zoneNames], out[, , "unknown"])

  return(out)
}
