#' @title convertCEDS_CMIP7
#'
#' @description converts harmonized emission data used for CMIP7, global data
#' is removed here, use ``readSource(CEDS_CMIP7, convert = FALSE)`` to obtain it.
#' @param x magpie object from source function
#' @return MAgPIE object
#' @author Pascal Weigmann

convertCEDS_CMIP7 <- function(x) {

  # add Kosovo to Serbia
  kos <- x["kos", , ]
  getItems(kos, dim = 1) <- "srb"
  x["srb", , ] <- x["srb", , ] + kos
  x <- x["kos", , , invert = TRUE]

  # remove global data, it has to be read in with convert = FALSE
  x <- x["GLO", , invert = TRUE]

  # fills missing ISO countries and remove unknown ISO countries
  getItems(x, dim = 1) <- toupper(getItems(x, dim = 1))
  x <- toolCountryFill(x, fill = NA)

  # aircraft and shipping only have global values, but while aircraft is NA on
  # country level, shipping has zeros
  x[, , "International Shipping", pmatch = TRUE] <- NA

  # filter out variables which are NA everywhere (those that had global values only)
  keep <- apply(!is.na(x), 3, any)
  x <- x[, , keep]
  x[is.na(x)] <- 0

  return(x)
}
