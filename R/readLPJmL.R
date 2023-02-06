#' @title readLPJmL
#' @description Read LPJmL content
#' @param subtype Switch between different input
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens, Abhijeet Mishra, Felicitas Beier
#' @seealso
#' [readLPJ()]
#' @examples
#' \dontrun{
#' readSource("LPJmL", subtype = "LPJmL5:CRU4p02.soilc", convert = "onlycorrect")
#' }
#'
#' @importFrom lpjclass readLPJ

readLPJmL <- function(subtype = "LPJmL5:CRU4p02.soilc") { # nolint: cyclocomp_linter.

  if (grepl("\\.", subtype)) {

    subtype     <- strsplit(gsub(":", "/", subtype), split = "\\.")
    folder      <- unlist(subtype)[1]
    subtype     <- unlist(subtype)[2]

  } else {
    stop("readLPJmL needs version and climatetype information")
  }

  files <- c(soilc              = "soilc_natveg.bin",
             soilc_layer        = "soilc_layer_natveg.bin",
             litc               = "litc_natveg.bin",
             vegc               = "vegc_natveg.bin",
             vegc_lpjcell       = "vegc_natveg.bin",
             alitfallc          = "alitfallc_natveg.bin",
             alitterfallc       = "alitterfallc_natveg.bin",
             alitfalln          = "alitfalln_natveg.bin",
             harvest            = "pft_harvest.pft.bin",
             irrig              = "cft_airrig.pft.bin",
             irrig_lpjcell      = "cft_airrig.pft.bin",
             cwater_b           = "cft_consump_water_b.pft.bin",
             cwater_b_lpjcell   = "cft_consump_water_b.pft.bin",
             sdate              = "sdate.bin",
             hdate              = "hdate.bin",
             transpiration      = "mtransp_natveg.bin",
             discharge          = "mdischarge_natveg.bin",
             discharge_lpjcell  = "mdischarge_natveg.bin",
             runoff             = "mrunoff_natveg.bin",
             runoff_lpjcell     = "mrunoff_natveg.bin",
             evaporation        = "mevap_natveg.bin",
             evap_lake          = "mevap_lake.bin",
             evap_lake_lpjcell  = "mevap_lake.bin",
             mevap_lake         = "mevap_lake.bin",
             mevap_lake_lpjcell = "mevap_lake.bin",
             input_lake         = "input_lake.bin",
             input_lake_lpjcell = "input_lake.bin",
             mtranspiration     = "mtransp_natveg.bin",
             mdischarge         = "mdischarge_natveg.bin",
             mdischarge_lpjcell = "mdischarge_natveg.bin",
             mrunoff            = "mrunoff_natveg.bin",
             mrunoff_lpjcell    = "mrunoff_natveg.bin",
             mevaporation       = "mevap_natveg.bin",
             vegc_grass         = "mean_vegc_mangrass.bin",
             litc_grass         = "litc_mangrass.bin",
             soilc_grass        = "soilc_mangrass.bin"
             )

  filename <- toolSubtypeSelect(subtype, files)

  if (tmp <- file.exists(file.path(folder, "tmp.out"))) {

    tmp        <- readLines(file.path(folder, "tmp.out"))
    years      <- as.numeric(unlist(regmatches(tmp, gregexpr("\\d{4}", tmp))))
    startYear  <- years[1]
    years      <- seq(years[1], years[2], 1)

  } else {
    # default
    startYear  <- 1901
    years      <- seq(startYear, 2017, 1)
  }

  unitTrans <- 0.01               # Transformation factor gC/m^2 --> t/ha

  if (grepl("soilc|litc|vegc|alitfallc|alitterfallc|alitfalln|vegc_grass|litc_grass|soilc_grass",
            subtype) && subtype != "soilc_layer") {
    startYear  <- startYear           # Start year of data set
    years      <- years                # Vector of years that should be exported
    nbands     <- 1                    # Number of bands in the .bin file
    avgRange   <- 1                    # Number of years used for averaging

    if (grepl("_lpjcell", subtype)) {
      x <- readLPJ(
        file_name = file.path(folder, filename),
        wyears = years,
        syear = startYear,
        averaging_range = avgRange,
        ncells = 67420,
        bands = nbands,
        soilcells = FALSE)
    } else {
      x <- readLPJ(
        file_name = file.path(folder, filename),
        wyears = years,
        syear = startYear,
        averaging_range = avgRange,
        bands = nbands,
        soilcells = TRUE)
    }

    # Transform to MAgPIE object
    if (grepl("_lpjcell", subtype)) {

      class(x)     <- "array"
      x            <- collapseNames(as.magpie(x, spatial = 1))
      mapLPJcell   <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",
                                     type = "cell", where = "mrcommons")
      getCells(x)  <- paste(mapLPJcell$ISO, 1:67420, sep = ".")
      names(dimnames(x))[1] <- paste0(names(dimnames(x))[1], ".region")

    } else {
      x <- collapseNames(as.magpie(x))
    }

    x           <- x * unitTrans
    getNames(x) <- subtype

  } else if (grepl("*date*", subtype)) {

    startYear  <- startYear           # Start year of data set
    years      <- years                # Vector of years that should be exported
    nbands     <- 24                   # Number of bands in the .bin file
    avgRange   <- 1                    # Number of years used for averaging

    x <- readLPJ(file_name = file.path(folder, filename),
                 wyears = years,
                 syear = startYear,
                 averaging_range = avgRange,
                 bands = nbands,
                 datatype = integer(),
                 bytes = 2,
                 soilcells = TRUE,
                 ncells = 67420)

    x <- collapseNames(as.magpie(x))

  } else if (subtype %in% c("soilc_layer")) {

    startYear  <- startYear           # Start year of data set
    years      <- years                # Vector of years that should be exported
    nbands     <- 5                    # Number of bands in the .bin file
    avgRange   <- 1                    # Number of years used for averaging

    x <- readLPJ(
      file_name = file.path(folder, filename),
      wyears = years,
      syear = startYear,
      averaging_range = avgRange,
      bands = nbands,
      soilcells = TRUE)

    x <- collapseNames(as.magpie(x))
    x <- x * unitTrans

    getNames(x)     <- paste0("soilc.", getNames(x))
    getSets(x)[4:5] <- c("data", "layer")

  } else if (grepl("transpiration|discharge|runoff|evaporation|evap_lake", subtype)) {

    startYear  <- startYear         # Start year of data set
    years       <- years              # Vector of years that should be exported
    nbands      <- 1                  # Number of bands in the .bin file
    avgRange   <- 1                  # Number of years used for averaging

    # monthly values
    if (grepl("_lpjcell", subtype)) {
      x <- readLPJ(
        file_name = file.path(folder, filename),
        wyears = years,
        syear = startYear,
        averaging_range = avgRange,
        monthly = TRUE,
        ncells = 67420,
        soilcells = FALSE)
    } else {
      x <- readLPJ(
        file_name = file.path(folder, filename),
        wyears = years,
        syear = startYear,
        averaging_range = avgRange,
        monthly = TRUE,
        soilcells = TRUE)
    }

    # unit transformation
    if (grepl("transpiration", subtype)) {
      # Transform units: liter/m^2 -> m^3/ha
      unitTransTRANSP <- 10
      x <- x * unitTransTRANSP

    } else if (grepl("discharge", subtype)) {
      # In LPJmL: (monthly) discharge given in hm3/d (= mio. m3/day)
      # Transform units of discharge: mio. m^3/day -> mio. m^3/month
      monthDays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      names(monthDays) <- dimnames(x)[[3]]
      for (month in names(monthDays)) {
        x[, , month, ] <- x[, , month, ] * monthDays[month]
      }

    } else if (grepl("runoff|evap_lake", subtype)) {
      # In LPJmL: (monthly) runoff given in LPJmL: mm/month
      if (grepl("_lpjcell", subtype)) {
        cb <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",
                             type = "cell", where = "mrcommons")
        cellArea <- (111e3 * 0.5) * (111e3 * 0.5) * cos(cb$lat / 180 * pi)
        class(x) <- "array"
        x        <- as.magpie(x, spatial = 1)
        # Transform units: liter/m^2 -> liter
        x <- x * cellArea
      } else {
        # Get cellular coordinate information and calculate cell area
        cb <- as.data.frame(magpie_coord)
        cellArea  <- (111e3 * 0.5) * (111e3 * 0.5) * cos(cb$lat / 180 * pi)
        # Transform units: liter/m^2 -> liter
        x <- as.magpie(x) * cellArea
      }
      # Transform units: liter -> mio. m^3
      x <- x / (1000 * 1000000)

    } else if (grepl("evaporation", subtype)) {
      # Transform units: liter/m^2 -> m^3/ha
      unitTransEVAP <- 10
      x <- x * unitTransEVAP

    }

    # Transform to MAgPIE object
    if (grepl("_lpjcell", subtype)) {

      class(x)     <- "array"
      x            <- collapseNames(as.magpie(x, spatial = 1))
      mapLPJcell   <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",
                                     type = "cell", where = "mrcommons")
      getCells(x)  <- paste(mapLPJcell$ISO, 1:67420, sep = ".")
      names(dimnames(x))[1] <- paste0(names(dimnames(x))[1], ".region")

    } else {
      x <- collapseNames(as.magpie(x))
    }

    if (grepl("layer", subtype)) {

      subtype          <- gsub("_", "\\.", subtype)       # Expand dimension to layers
      getNames(x)      <- paste0(subtype, ".", getNames(x))
      getSets(x)[4:6]  <- c("data", "layer", "month")

    } else {
      getNames(x)      <- paste0(subtype, ".", getNames(x))
      getSets(x)[4:5]  <- c("data", "month")
    }

    # Annual value (total over all month)
    if (!grepl("^m", subtype)) {
      x <- dimSums(x, dim = "month")
    }

  } else if (grepl("*harvest*", subtype)) {

    startYear <- startYear           # Start year of data set
    years     <- years                # Vector of years that should be exported
    nbands    <- 32                   # Number of bands in the .bin file
    avgRange  <- 1                    # Number of years used for averaging

    x <- readLPJ(
      file_name = file.path(folder, filename),
      wyears = years,
      syear = startYear,
      averaging_range = avgRange,
      bands = nbands,
      soilcells = TRUE)

    # Transformation factor gC/m^2 --> t/ha
    yieldTrans <- 0.01 / 0.45
    x <- collapseNames(as.magpie(x))
    x <- x * yieldTrans

  } else if (grepl("irrig|cwater_b", subtype)) {

    startYear  <- startYear           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 32                   # Number of bands in the .bin file
    avgRange    <- 1                    # Number of years used for averaging

    if (grepl("_lpjcell", subtype)) {
      x <- readLPJ(
        file_name = file.path(folder, filename),
        wyears = years,
        syear = startYear,
        averaging_range = avgRange,
        bands = nbands,
        ncells = 67420,
        soilcells = FALSE)
    } else {
      x <- readLPJ(
        file_name = file.path(folder, filename),
        wyears = years,
        syear = startYear,
        averaging_range = avgRange,
        bands = nbands,
        soilcells = TRUE)
    }

    if (grepl("_lpjcell", subtype)) {

      class(x)     <- "array"
      x            <- collapseNames(as.magpie(x, spatial = 1))
      mapLPJcell   <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",
                                     type = "cell", where = "mrcommons")
      getCells(x)  <- paste(mapLPJcell$ISO, 1:67420, sep = ".")
      names(dimnames(x))[1] <- paste0(names(dimnames(x))[1], ".region")

    } else {
      x <- collapseNames(as.magpie(x))
    }
    # Transform units (transform from: mm per year = liter per m^2 transform to: m^3 per ha)
    # 1 000 liter   = 1 m^3
    # 10 000 m^2    = 1 ha
    # 1 liter/m^2   = 10 m^3/ha
    # -> mm/yr * 10 = m^3/ha
    irrigTransform  <- 10
    x[, , "irrigated"] <- x[, , "irrigated"] * irrigTransform # units are now: m^3 per ha per year

  } else if (grepl("input_lake", subtype)) {

    startYear  <- startYear           # Start year of data set
    years      <- years                # Vector of years that should be exported
    nbands     <- 1                    # Number of bands in the .bin file
    avgRange   <- 1                    # Number of years used for averaging

    if (grepl("_lpjcell", subtype)) {
      x <- readLPJ(
        file_name = file.path(folder, filename),
        wyears = years,
        syear = startYear,
        averaging_range = avgRange,
        bands = nbands,
        ncells = 67420,
        soilcells = FALSE)
    } else {
      x <- readLPJ(
        file_name = file.path(folder, filename),
        wyears = years,
        syear = startYear,
        averaging_range = avgRange,
        bands = nbands,
        soilcells = TRUE)
    }

    if (grepl("_lpjcell", subtype)) {

      class(x)     <- "array"
      x            <- collapseNames(as.magpie(x, spatial = 1))
      mapLPJcell <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",
                                     type = "cell", where = "mrcommons")
      getCells(x)  <- paste(mapLPJcell$ISO, 1:67420, sep = ".")
      names(dimnames(x))[1] <- paste0(names(dimnames(x))[1], ".region")

    } else {
      x <- collapseNames(as.magpie(x))
    }
    getNames(x) <- subtype

  } else {
    stop(paste0("subtype ", subtype, " is not existing"))
  }

  return(x)

}
