#' @title convertCEDS2025
#'
#' @description converts emission data from the CEDS database
#' @param x magpie object from source function
#' @return MAgPIE object
#' @author Benjamin Leon Bodirsky, David Klein

convertCEDS2025 <- function(x) {

  # fill all missing countries with 0
  x[is.na(x)] <- 0

  # change unit to Mt
  x <- x / 1000

  x1 <- x["srb (kosovo)", , ]
  getItems(x1, dim = 1) <- "srb"
  x["srb", , ] <- x["srb", , ] + x1
  x          <- x[c("srb (kosovo)"), , , invert = TRUE]

  getItems(x, dim = 1) <- gsub("global", "glo", getItems(x, dim = 1))
  getItems(x, dim = 1) <- toupper(getItems(x, dim = 1))

  map2 <-
    c(BC_ktC = "bc_c",
      CO_ktCO = "co",
      CH4_ktCH4 = "ch4",
      N2O_ktN2O = "n2o_n",
      NH3_ktNH3 = "nh3_n",
      NOx_ktNO2 = "no2_n",
      NMVOC_ktNMVOC = "nmvoc",
      OC_ktC = "oc_c",
      SO2_ktSO2 = "so2",
      CO2_ktCO2 = "co2_c")
  getNames(x, dim = 2) <- map2[getNames(x, dim = 2)]

  x[, , "n2o_n"] <- x[, , "n2o_n"] / 44 * 28
  x[, , "nh3_n"] <- x[, , "nh3_n"] / 17 * 14
  x[, , "no2_n"] <- x[, , "no2_n"] / 46 * 14
  x[, , "co2_c"] <- x[, , "co2_c"] / 44 * 12

  # international shipping and aviation data is global only.
  # We want to distribute it evenly across all countries for now, so it will not
  # be removed by toolCountryfill.
  varGlob <- c("1A3di_International-shipping",
               "1A3ai_International-aviation")

  xGLO <- x["GLO", , varGlob]
  x <- x["GLO", , invert = TRUE]

  # fills missing ISO countires and remove unknown ISO countries
  x <- toolCountryFill(x, fill = 0)

  # Create weight 1 for xGLO
  w <- new.magpie(getItems(x, dim = 1),
                  getItems(x, dim = 2),
                  getItems(xGLO, dim = 3),
                  fill = 1)

  # Create mapping of each country to GLO
  mapping <- data.frame(from = getItems(x, dim = 1), to = "GLO")

  # Spread global shipping and aviation data evenly across countries and save it
  # to regions of x
  x[, , varGlob] <- toolAggregate(xGLO, mapping, weight = w)

  return(x)
}
