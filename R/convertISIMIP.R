#' @title convertISIMIP
#' @description convert data to ISO country level
#'
#' @param x MAgPIE object on cellular level
#' @param subtype data subtype
#'
#' @importFrom magclass collapseNames collapseDim dimSums
#'
#' @return MAgPIE object on country level
#'
#' @author Jan Philipp Dietrich, Felicitas Beier
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource("ISIMIP", convert = TRUE)
#' }
#'
convertISIMIP <- function(x, subtype) {
  if (grepl("^airww", subtype)) {
    landarea <- setYears(collapseNames(dimSums(readSource("LUH2v2", subtype = "states",
                                                          convert = "onlycorrect")[, "y1995", ], dim = 3)), NULL)
    landarea <- collapseDim(landarea, dim = "iso")
    weight   <- landarea
    weight   <- add_columns(weight, dim = 1, addnm = "178p75.-49p25", fill = 0) # add missing weight
  } else {
    stop("Aggregation rule for given subtype \"", subtype, "\" not defined!")
  }
  return(toolAggregateCell2Country(x, weight = weight, fill = 0))
}
