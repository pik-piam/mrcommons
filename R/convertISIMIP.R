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
    landarea <- setYears(collapseNames(dimSums(readSource("LUH2v2", subtype = "states_1995to1996",
                                                          convert = "onlycorrect")[, "y1995", ],
                                                dim = 3)),
                         NULL)
    landarea <- collapseDim(landarea, dim = "iso")
    x        <- collapseDim(x, dim = "region")

  } else {
    stop("Aggregation rule for given subtype \"", subtype, "\" not defined!")
  }

  return(toolAggregateCell2Country(x, weight = landarea, fill = 0))

}
