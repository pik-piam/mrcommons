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
#' @seealso [madrat::readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource("ISIMIP", convert = TRUE)
#' }
#'
convertISIMIP <- function(x, subtype) {

  if (grepl("^airww", subtype)) {
    # read in LUH landarea as weight
    landarea <- collapseNames(dimSums(readSource("LUH2v2", subtype = "states_1995to1996",
                                                 convert = "onlycorrect")[, "y1995", ],
                                      dim = 3))
    # add small amount to avoid zero weight
    landarea <- setYears(landarea, NULL) + 1e-10
    # clean up object
    landarea <- collapseDim(landarea, dim = "iso")
    x        <- collapseDim(x, dim = "region")

  } else {
    stop("Aggregation rule for given subtype \"", subtype, "\" not defined!")
  }

  return(toolAggregateCell2Country(x, weight = landarea, fill = 0))

}
