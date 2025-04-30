#' @title readACCMIP
#' @description Read in data from the Atmospheric Chemistry and Climate Model Intercomparison Project
#'
#' @param subtype data subtype. available subtypes are:
#' \itemize{
#' \item nhx_1850
#' \item noy_1850
#' \item sox_1850
#' \item nhx_1980
#' \item noy_1980
#' \item sox_1980
#' \item nhx_2000
#' \item noy_2000
#' \item sox_2000
#' \item nhx_26_2030
#' \item nhx_45_2030
#' \item nhx_85_2030
#' \item noy_26_2030
#' \item noy_45_2030
#' \item noy_85_2030
#' \item sox_26_2030
#' \item sox_45_2030
#' \item sox_85_2030
#' \item nhx_26_2100
#' \item nhx_45_2100
#' \item nhx_85_2100
#' \item noy_26_2100
#' \item noy_45_2100
#' \item noy_85_2100
#' \item sox_26_2100
#' \item sox_45_2100
#' \item sox_85_2100
#' }
#' @return magpie object of the ACCMIP data. Unit is t NO2-N  per ha per year, or t NH3-N  per ha per year,...
#' @author Roman Popov
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readACCMIP("ACCMIP", "nhx_2000")
#' a <- readACCMIP("ACCMIP", "sox_26_2030")
#' }
#' @importFrom magclass read.magpie hasCoords
#' @importFrom stringr str_sub
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 nc_close
#'
readACCMIP <- function(subtype = NULL) {

  if (substr(subtype, 1, 4) == "glo_") {
    glo <- TRUE
    subtype <- substring(subtype, 5)
  } else {
    glo <- FALSE
  }
  files <- c(
    nhx_1850 = "accmip_nhx_acchist_1850.nc",
    noy_1850 = "accmip_noy_acchist_1850.nc",
    sox_1850 = "accmip_sox_acchist_1850.nc",
    nhx_1980 = "accmip_nhx_acchist_1980.nc",
    noy_1980 = "accmip_noy_acchist_1980.nc",
    sox_1980 = "accmip_sox_acchist_1980.nc",
    nhx_2000 = "accmip_nhx_acchist_2000.nc",
    noy_2000 = "accmip_noy_acchist_2000.nc",
    sox_2000 = "accmip_sox_acchist_2000.nc",
    nhx_26_2030 = "accmip_nhx_accrcp26_2030.nc",
    nhx_45_2030 = "accmip_nhx_accrcp45_2030.nc",
    nhx_85_2030 = "accmip_nhx_accrcp85_2030.nc",
    noy_26_2030 = "accmip_noy_accrcp26_2030.nc",
    noy_45_2030 = "accmip_noy_accrcp45_2030.nc",
    noy_85_2030 = "accmip_noy_accrcp85_2030.nc",
    sox_26_2030 = "accmip_sox_accrcp26_2030.nc",
    sox_45_2030 = "accmip_sox_accrcp45_2030.nc",
    sox_85_2030 = "accmip_sox_accrcp85_2030.nc",
    nhx_26_2100 = "accmip_nhx_accrcp26_2100.nc",
    nhx_45_2100 = "accmip_nhx_accrcp45_2100.nc",
    nhx_85_2100 = "accmip_nhx_accrcp85_2100.nc",
    noy_26_2100 = "accmip_noy_accrcp26_2100.nc",
    noy_45_2100 = "accmip_noy_accrcp45_2100.nc",
    noy_85_2100 = "accmip_noy_accrcp85_2100.nc",
    sox_26_2100 = "accmip_sox_accrcp26_2100.nc",
    sox_45_2100 = "accmip_sox_accrcp45_2100.nc",
    sox_85_2100 = "accmip_sox_accrcp85_2100.nc"
  )

  file <- toolSubtypeSelect(subtype, files)

  if (glo == FALSE) {

    a <- read.magpie(file_name = file, file_type = "nc")
    d <- toolCoord2Isocoord(a)

    getYears(d) <- c(paste("y", str_sub(file, -7, -4), sep = ""))
    # kg(N)/m²/s to t(N)/ha/y
    # 1t(N)/ha/y = 1000kg(N) / 10.000m²/ (3600*24*365)s = 1000kg(N) / 10.000m² / 31536000s
    e <- d[, , "area", invert = TRUE] * 315360000

    getNames(e) <- sub(getNames(e), pattern = "_", replacement = ".")
    getNames(e) <- sub(getNames(e), pattern = "noy", replacement = "no2_n")
    getNames(e) <- sub(getNames(e), pattern = "nhx", replacement = "nh3_n")
    f <- add_columns(e, dim = 3.1, addnm = "area")
    f[, , "area"] <- d[, , "area"] / 10^12 * 100 # transform from square km to ha

    out <- clean_magpie(f)

    if ("no2_n_std" %in% c(getNames(out, dim = 2))) {
      out <- out[, , "no2_n_std", invert = TRUE]
    }

    return(out)

  } else {

    getGlobSum <- function(ncFile) {

      ncFileData <- nc_open(ncFile)

      sums <- vector(length = ncFileData$nvars)
      dataNames <- vector(length = ncFileData$nvars)

      dataArea <- ncvar_get(ncFileData, varid = "area")

      for (i in 1:ncFileData$nvars) {
        dataVals <- ncvar_get(ncFileData, varid = names(ncFileData$var)[i])
        dataVals[dataVals == -9999] <- 0
        # 1t(N)/ha/y = 1000kg(N) / 10.000m²/ (3600*24*365)s = 1000kg(N) / 10.000m² / 31536000s
        sums[i] <- sum(dataVals * dataArea * 31536)

        dataNames[i] <- c(ncFileData$var[[i]]$name)
      }

      names(sums) <- dataNames

      nc_close(ncFileData)

      return(sums[-length(sums)])
    }

    out <- as.magpie(getGlobSum(file)) / 10^6

    getYears(out) <- c(paste("y", str_sub(file, -7, -4), sep = ""))
    getNames(out) <- sub(getNames(out), pattern = "_", replacement = ".")
    getNames(out) <- sub(getNames(out), pattern = "noy", replacement = "no2_n")
    getNames(out) <- sub(getNames(out), pattern = "nhx", replacement = "nh3_n")

    if ("no2_n_std" %in% c(getNames(out, dim = 2))) {
      out <- out[, , "no2_n_std", invert = TRUE]
    }

    return(clean_magpie(out))
  }
}
