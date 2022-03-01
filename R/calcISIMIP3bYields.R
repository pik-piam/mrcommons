#' @title calcISIMIP3bYields
#'
#' @description reads and cleans up ISIMIP3b crop yield data
#'
#' @param subtype subtype of yield based on readISIMIPoutputs, for crop yields
#' @param cells magpie or lpjcell
#' @param smooth smooth cells via spline
#'
#' @return magpie object in cellular resolution
#' @author David Meng-Chuen Chen
#'
#' @examples
#' \dontrun{
#' calcOutput("ISIMIP3bYields", aggregate = FALSE)
#' }
#'
#' @importFrom mstools toolHoldConstant


calcISIMIP3bYields <- function(subtype = "yields:EPIC-IIASA:ukesm1-0-ll:ssp585:default:3b",
                               smooth = TRUE, cells = "magpiecell") {

if (grepl("historical", subtype)) {
  stop("Can only read full future scenarios for now, with historical already added")
}

  st <- toolSplitSubtype(subtype, list(dataset = "yields",
                                      model   = c("LPJmL", "EPIC-IIASA", "pDSSAT", "CYGMA1p74"),
                                      gcm     = c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll"),
                                      scen    = c("historical", "ssp126", "ssp370", "ssp585"),
                                      co2     = c("default", "2015co2"),
                                      version = c("2a", "2b", "3a", "3b")))

  pastSubtype <- paste(st$dataset, st$model, st$gcm, "historical", st$co2, st$version, sep = ":")
  past <- readSource("ISIMIP", subtype = pastSubtype, convert = FALSE)
  scen <- readSource("ISIMIP", subtype = subtype, convert = FALSE)

  ### last year of both are chopped off due to GGCMI processing, dont use and rather interpolate 2014 and hold 2100 constant
  past <- past[, 2014, inv = TRUE]
  scen <- scen[, 2100, inv = TRUE]

  x <- mbind(past, scen)
  x <- toolCoord2Isocell(x, cells = cells)

  # pdssat has reverse naming and extra yields name ## These special cases need to be cleaned up
  if (st$model == "pDSSAT") {
  x <- collapseNames(x)
  x <- dimOrder(x = x, perm = c(2, 1))
}
  if (st$model == "LPJmL") {
    x <- collapseNames(x)
    x <- dimOrder(x = x, perm = c(2, 1))
    }

  x[is.na(x)] <- 0


  # take higher yielding variety  based on highest mean yield between 1981 and 2011
  if (st$model == "CYGMA1p74") { # CYGMA has no winter wheat
          getNames(x, dim = 1)[getNames(x, dim = 1) == "springwheat"] <- "tece"
  } else {
  higherw <- magpply(x[, 1981:2011, "springwheat", ],
                     FUN = mean, MARGIN = c(1, 3)) > magpply(x[, 1981:2011, "winterwheat", ],
                                                             FUN = mean, MARGIN = c(1, 3))
  higherw <- time_interpolate(setYears(higherw, 1961),
                              interpolated_year = getYears(x),
                              integrate_interpolated_years = TRUE)
  wheat <- ifelse(higherw == 1, x[, , "springwheat", ], x[, , "winterwheat", ])
  wheat <- add_dimension(collapseNames(wheat), dim = 3.1, nm = "tece")
  x <- x[, , c("springwheat", "winterwheat"), inv = TRUE]
  x <- mbind(x, wheat)
 }
  higherr <- magpply(x[, 1981:2011, "ricea", ],
                     FUN = mean, MARGIN = c(1, 3)) > magpply(x[, 1981:2011, "riceb", ],
                                                             FUN = mean, MARGIN = c(1, 3))
  higherr <- time_interpolate(setYears(higherr, 1961),
                              interpolated_year = getYears(x),
                              integrate_interpolated_years = TRUE)
  rice <- ifelse(higherr == 1, x[, , "ricea", ], x[, , "riceb", ])
  rice <- add_dimension(collapseNames(rice), dim = 3.1, nm = "rice_pro")
  x <- x[, , c("ricea", "riceb"), inv = TRUE]
  x <- mbind(x, rice)

  if (smooth == TRUE) {
  # smooth with spline
  x <- toolSmooth(x)
}
  # set very small yields from smoothing to 0

  ### here interpolate 2014 and hold 2100 constant (after smoothing)
  x <- time_interpolate(x, interpolated_year = 2014, integrate_interpolated_years = TRUE)
  x <- toolHoldConstant(x, 2100)


  getNames(x, dim = 1)[getNames(x, dim = 1) == "soy"] <- "soybean"
  getNames(x, dim = 1)[getNames(x, dim = 1) == "maize"] <- "maiz"
  getNames(x, dim = 2)[getNames(x, dim = 2) == "fullyirrigated"] <- "irrigated"
  getNames(x, dim = 2)[getNames(x, dim = 2) == "noirrigation"] <- "rainfed"
  getNames(x, dim = 2)[getNames(x, dim = 2) == "firr"] <- "irrigated"
  getNames(x, dim = 2)[getNames(x, dim = 2) == "noirr"] <- "rainfed"

  crop_area_weight     <- dimSums(calcOutput("Croparea", sectoral = "kcr", physical = TRUE, irrigation = FALSE,
                                             cellular = TRUE, cells = cells, aggregate = FALSE, years = "y1995",
                                             round = 6)[, , getNames(x, dim = 1)],
                                  dim = 3)

  return(list(x            = x,
              weight       = crop_area_weight,
              unit         = "t/ha",
              description  = "ISIMIP3b GGCMI yields for soy rice wheat maize",
              isocountries = FALSE))
}
