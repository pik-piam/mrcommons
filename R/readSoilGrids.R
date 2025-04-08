#' @title readSoilGrids
#' @description This function reads the raw SoilGrids data (available at
#'              https://files.isric.org/soilgrids/data/recent/OCSTHA_M_30cm_250m_ll.tif)
#'              or if available the preprocessed raster layers.
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#' @param subtype  Switch between different input. Use predefined ones or any FileName
#'                 specified in 'SoilGrids/META_GEOTIFF_1B.csv'
#'
#' @seealso
#' [downloadSoilGrids()]
#'
#' @examples
#' \dontrun{
#' readSource("SoilGrids", subtype = "cstock_0_30")
#' }
#'
#' @importFrom raster raster aggregate res res<- projectRaster writeRaster as.matrix

readSoilGrids <- function(subtype) {

  if (grepl(".tif", subtype)) {

    file <- subtype

  } else {

    files <- c(cstock_0_30     = "OCSTHA_M_30cm_250m_ll.tif",
               # Soil organic carbon stock in tons per ha for depth interval 0.00 m - 0.30 m

               # for new SoilGrids in 0.5x0.5 resolution
               cstock_0_30_new = "ocs_0-30cm_mean.tif",
               # Soil organic carbon stock in tons per ha for depth interval 0.00 m - 0.30 m
               cstock_0_30_q05_new = "ocs_0-30cm_Q0p05.tif",
               #  5% quantile for soil organic carbon stock in tons per ha for depth interval 0.00 m - 0.30 m
               cstock_0_30_q95_new = "ocs_0-30cm_Q0p95.tif",
               # 95% quantile for soil organic carbon stock in tons per ha for depth interval 0.00 m - 0.30 m
               cstock_0_30_sig_new = "ocs_0-30cm_uncertainty.tif",
               # uncertainty for Soil organic carbon stock in tons per ha for depth interval 0.00 m - 0.30 m

               sandfrac_0      = "SNDPPT_M_sl1_250m_ll.tif",
               # Sand content (50-2000 micro meter) mass fraction in % at depth 0.00 m
               sandfrac_5      = "SNDPPT_M_sl2_250m_ll.tif",
               # Sand content (50-2000 micro meter) mass fraction in % at depth 0.05 m
               sandfrac_15     = "SNDPPT_M_sl3_250m_ll.tif",
               # Sand content (50-2000 micro meter) mass fraction in % at depth 0.15 m
               sandfrac_30     = "SNDPPT_M_sl4_250m_ll.tif")
    # Sand content (50-2000 micro meter) mass fraction in % at depth 0.30 m

    file <- toolSubtypeSelect(subtype, files)
  }

  if (!file.exists(paste0(subtype, ".grd"))) {

    tmp <- raster(file)

    if (grepl("new", subtype)) {
      res(tmp) <- c(0.5, 0.5)
      out <- tmp
    } else {
      out <- aggregate(tmp, fact = 240, fun = mean,  na.rm = TRUE)
      writeRaster(out, filename = paste0(subtype, ".grd"))
    }

  } else {

    out <- raster(paste0(subtype, ".grd"))
  }

  # Load celliso names for 1:67420 magpie cells
  map       <- toolGetMappingCoord2Country(pretty = TRUE)

  # Change longitude and latitude
  r50   <- raster(resolution = 0.5)
  mag   <- projectRaster(out, r50, over = TRUE)
  mag   <- as.magpie(terra::extract(mag, map[c("lon", "lat")]), spatial = 1)

  getNames(mag) <- subtype
  getCells(mag) <- paste(map$coords, map$iso, sep = ".")
  getYears(mag) <- "y2000"
  getSets(mag)  <- c("x.y.iso", "t", "data")

  return(mag)
}
