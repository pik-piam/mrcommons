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
    luh3 <- calcOutput("LUH3", cellular = TRUE, yrs = 1995, aggregate = FALSE)
    landarea <- dimSums(luh3, 3)
    landarea <- collapseDim(landarea, dim = "iso") + 10^-10

    x        <- collapseDim(x, dim = "region")

  } else {
    stop("Aggregation rule for given subtype \"", subtype, "\" not defined!")
  }

  return(toolAggregateCell2Country(x, weight = landarea, fill = 0))

}
