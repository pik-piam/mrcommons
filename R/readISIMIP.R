#' @title readISIMIP
#' @description Reads in ISIMIP data
#' @param subtype Type of ISIMIP data that should be read.
#' It consists of variable ("airrww"),
#' model ("cwatm","h08","lpjml","matsiro","mpi-hm","pcr-globwb"),
#' GCM ("ipsl-cm5a-lr","gfdl-esm2m","miroc5","hadgem2-es")
#' and database version ("2a","2b","3a","3b"), separated by ":"
#' (e.g. "airww:LPJmL:gfdl-esm2m:2b")
#' Similaryly for ISIMIP GGCMI phase3b data,  with scenarios and CO2 fert setting,
#' downloads for all crops and irrigation settings
#' models ("LPJmL", "EPIC-IIASA", "pDSSAT", "CYGMA1p74"),
#' gcms ("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll"),
#' scenarios  ("historical", "ssp126", "ssp370", "ssp585"),
#' co2 ("default", "2015co2"),
#' version c("2a","2b","3a","3b")))
#' Example of yield subtype : "yields:EPIC-IIASA:ukesm1-0-ll:ssp585:default:3b"
#' @return MAgPIE object with the requested data
#' @author Jan Philipp Dietrich, Felicitas Beier, David Chen
#' @note Values for years before 1961 will be ignored to reduce overall object size
#' @examples
#' \dontrun{
#' readSource("ISIMIP", convert = TRUE)
#' }
#'
#' @importFrom magclass getCoords time_interpolate
#' @importFrom raster brick subset stack
readISIMIP <- function(subtype = "airww:LPJmL:gfdl-esm2m:2b") {

  # map of 67420 grid cells
  map <- toolGetMappingCoord2Country()

  if (grepl("airww", subtype)) {

    .timevector <- function(start, end) {
      years      <- paste0("y", c(start:end))
      months    <- c("jan", "feb", "mar", "apr", "may", "jun",
                     "jul", "aug", "sep", "oct", "nov", "dec")
      return(paste0(rep(years, each = 12), ".", months))
    }

    file <- Sys.glob("*.nc4")
    if (length(file) != 1) stop("Not able to identify input file!")
    years <- tail(strsplit(sub("\\..*$", "", file), split = "_")[[1]], 2)

    r        <- suppressWarnings(brick(file))
    names(r) <- .timevector(years[1], years[2])
    r        <- subset(r, .timevector(max(years[1], 1961), years[2]))

    x <- as.magpie(r, temporal = 1)
    getSets(x, fulldim = FALSE)[2] <- "year.month"
    # add missing cell
    x <- add_columns(x, dim = 1,
                     addnm = setdiff(map$coords, getItems(x, dim = 1)),
                     fill = 0)

  } else if (grepl("yield", subtype)) {

    files <- Sys.glob("*.nc")
    if (length(file) < 1) stop("Not able to identify input file!")

    if (grepl("LDNDC", subtype, fixed = TRUE)) {

      r      <- NULL
      layers <- c("soy-noirr", "soy-firr", "wwh-noirr", "wwh-firr", "swh-noirr", "swh-firr",
                  "ri2-noirr", "ri2-firr", "ri1-noirr", "ri1-firr", "mai-noirr", "mai-firr")

      for (f in files) {
        for (la in layers) {
          if (grepl(la, f, fixed = TRUE)) {
            rAUX <- suppressWarnings(stack(f))
            names(rAUX) <- paste0("yield.", la, ".",
                                  if (grepl("historical", subtype)) 1:165 else 1:86)

            r <- if (is.null(r)) rAUX else stack(r, rAUX)
            rAUX <- NULL
          }
        }
      }
    } else {
      r <- suppressWarnings(stack(files))
    }

    # raster renaming
    names(r) <- sub("^(.*)\\.(.*)$", "\\2..\\1", names(r))
    names(r) <- gsub("X", "y", names(r))
    names(r) <- gsub("yield\\.", "", names(r))
    names(r) <- gsub("(\\.)(irr)", "\\2", names(r))
    names(r) <- gsub("(\\.)(wheat)", "\\2", names(r))
    names(r) <- gsub("(_wheat)", "wheat", names(r))
    names(r) <- gsub("rice1", "riceA", names(r), ignore.case = TRUE)
    names(r) <- gsub("rice2", "riceB", names(r), ignore.case = TRUE)

    # subset to year 1961 (1849+112) for faster processing
    if (grepl("historical", subtype)) {
      r <- subset(r, which(as.numeric(gsub("\\D+", "", names(r))) > 110))
      offset <- 1849
    } else {
      offset <- 2014
    }

    x <- as.magpie(r, spatial = 1)

    getNames(x) <- tolower(getNames(x))
    getYears(x) <- getYears(x, as.integer = TRUE) + offset

    # check in case the naming subseting does not work
    yearsSub <- getYears(x, as.integer = TRUE)
    yearsSub <- yearsSub[yearsSub >= 1960]
    x <- magpiesort(x[, yearsSub, ])

    # fill missing cells with 0 if any
    missingCells <- setdiff(map$coords, getItems(x, dim = 1))
    fill         <- new.magpie(cells_and_regions = missingCells,
                               years = getYears(x),
                               names = getNames(x),
                               fill = 0)
    x    <- suppressWarnings(mbind(x, fill))

    nameClean <- function(x, subtype, order = FALSE) {

      if ((grepl("pDSSAT", subtype) || grepl("LPJmL", subtype)) && order) {
        x <- collapseNames(x)
        x <- dimOrder(x = x, perm = c(2, 1))
      }

      getNames(x, dim = 1)[getNames(x, dim = 1) == "mai" |
                             getNames(x, dim = 1) == "maize" |
                             getNames(x, dim = 1) == "Maize"] <- "maiz"

      getNames(x, dim = 1)[getNames(x, dim = 1) == "soy" |
                             getNames(x, dim = 1) == "Soybean"] <- "soybean"

      getNames(x, dim = 1)[getNames(x, dim = 1) == "ri1" |
                             getNames(x, dim = 1) == "riceA"] <- "ricea"

      getNames(x, dim = 1)[getNames(x, dim = 1) == "ri2" |
                             getNames(x, dim = 1) == "riceB"] <- "riceb"

      getNames(x, dim = 1)[getNames(x, dim = 1) == "swh" |
                             getNames(x, dim = 1) == "Springwheat"] <- "springwheat"

      getNames(x, dim = 1)[getNames(x, dim = 1) == "wwh" |
                             getNames(x, dim = 1) == "Winterwheat"] <- "winterwheat"

      getNames(x, dim = 2)[getNames(x, dim = 2) == "fullyirrigated" |
                             getNames(x, dim = 2) == "firr" |
                             getNames(x, dim = 2) == "ir"] <- "irrigated"

      getNames(x, dim = 2)[getNames(x, dim = 2) == "noirrigation" |
                             getNames(x, dim = 2) == "noirr" |
                             getNames(x, dim = 2) == "rf"] <- "rainfed"

      return(x)
    }

    x <- nameClean(x, subtype, order = TRUE)
    x <- x[map$coords, , ]

    # Harvest year correction. If maturity day<planting date values correspond to y+1
    plantDay    <- readSource("GGCMICropCalendar",
                              subtype = "planting_day")[, , c("ri1", "ri2", "wwh",
                                                              "swh", "soy", "mai")][, , c("rf", "ir")]
    plantDay    <- collapseNames(plantDay)[map$coords, , ]
    maturityDay <- readSource("GGCMICropCalendar",
                              subtype = "maturity_day")[, , c("ri1", "ri2", "wwh",
                                                              "swh", "soy", "mai")][, , c("rf", "ir")]
    maturityDay <- collapseNames(maturityDay)[map$coords, , ]

    diff <- maturityDay - plantDay
    diff <- nameClean(diff, subtype, order = FALSE)
    xCorrected <- x
    xCorrected[is.na(x)] <- 0

    for (n in getNames(x)) {
      cellsCorr <- where(diff[, , n] < 0)$true$regions
      xCorrected[cellsCorr, 2:length(getYears(x)), n] <- setYears(xCorrected[cellsCorr, 1:(length(getYears(x)) - 1), n],
                                                                  getYears(xCorrected[cellsCorr,
                                                                                      2:length(getYears(x)), n]))
      xCorrected[cellsCorr, 1, n] <- NA
    }

    x <- xCorrected
    getSets(x) <- c("x", "y", "iso", "year", "data")
  }

  # naming of cell dimension (first sort)
  x <- x[map$coords, , ]
  getItems(x, dim = 1, raw = TRUE) <- paste(map$coords, map$iso, sep = ".")

  return(x)
}
