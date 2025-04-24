#' @title readTNC2019
#' @description Reads geospatial data from 'the nature conservancy' on ecoregions,
#'               major habitat types (MHT, or biome types) and biogeographic realms.
#' @return Returns magpie object with a share of each spatial unit belonging to
#'          a biogeographic realm and major habitat type.
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' readSource("TNC2019", convert = "onlycorrect")
#' }
#' @importFrom terra rast vect classify focal app segregate terraOptions tmpFiles rasterize
#' @importFrom withr local_tempdir defer
#'

readTNC2019 <- function() {

  # Set up 'terra' options
  terraOptions(tempdir = local_tempdir(tmpdir = getConfig("tmpfolder")), todisk = TRUE, memfrac = 0.5)
  defer(terraOptions(tempdir = tempdir()))

  # create a reference raster with a spatial resolution of 0.5°
  refRast <- rast(resolution = 0.5)

  # read vector data
  tncShp <- vect("./terr-ecoregions-TNC/tnc_terr_ecoregions.shp")

  # get ID of each biome
  biomeId <- unique(tncShp$RealmMHT)

  # rasterize each biome of each biogeographic realm
  biomeRealmRast <- rast()
  for (i in seq_len(length(biomeId))) {
    br <- tncShp[tncShp$RealmMHT %in% biomeId[i], "RealmMHT"]
    br <- suppressWarnings(rasterize(br, refRast, cover = FALSE))
    br <- classify(br, cbind(1, i))
    biomeRealmRast <- suppressWarnings(c(biomeRealmRast, br))
  }

  ## make sure that all magclass cells are covered along the sea coast
  # sum layers to a single raster
  biomeRealmRast <- app(biomeRealmRast, sum, na.rm = TRUE)
  # expand data along coastlines (NA cells only)
  biomeRealmRast <- focal(biomeRealmRast, w = 9, fun = "modal", na.rm = TRUE, na.policy = "only")
  # divide data into layers again
  biomeRealmRast <- segregate(biomeRealmRast, classes = seq_len(length(biomeId)))
  names(biomeRealmRast) <- biomeId

  # get spatial mapping
  map <- toolGetMappingCoord2Country(pretty = TRUE)
  # transform raster to magpie object
  biomeMag <- NULL
  for (m in biomeId) {
    biomeMag <- mbind(
      biomeMag,
      as.magpie(raster::extract(biomeRealmRast[[m]], map[c("lon", "lat")])[, m], spatial = 1)
    )
  }

  dimnames(biomeMag) <- list(
    "x.y.iso" = paste(map$coords, map$iso, sep = "."),
    "t" = NULL,
    "data" = names(biomeRealmRast)
  )

  out <- biomeMag

  return(out)
}
