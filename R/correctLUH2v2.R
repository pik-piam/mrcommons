#' @title correctLUH2v2
#' @description Correct LUH2v2 content
#'
#' @param x       magpie object provided by the read function
#' @param subtype switch between different inputs
#'
#' @return List of magpie object with results on cellular level
#'
#' @author Florian Humpenoeder, Stephen Wirth, Kristine Karstens, Felicitas Beier, Jan Philipp Dietrich,
#'  Edna J. Molina Bacca
#'
#' @importFrom magclass getCells
#'
correctLUH2v2 <- function(x, subtype) {

  if (any(is.na(x))) {
    vcat(verbosity = 1, paste(sum(is.na(x)) / length(x) * 100, "% of data points with NAs in LUH2. set to 0."))
    x[is.na(x)] <- 0
  }
  if (any(x < 0)) {
    vcat(verbosity = 1, paste(sum(x < 0) / length(x) * 100, "% of data points with negative values in LUH2. set to 0."))
    x[x < 0] <- 0
  }

  years <- getYears(x, as.integer = TRUE)

  if (grepl("states", subtype) &&
      length(intersect(2001:2015, years)) > 0 &&
      2000 %in% years &&
      2005 %in% years) {

    # check, if in JPN pasture+rangeland is unnaturally low
    if (sum(x["JPN", "y2005", c("pastr", "range")]) < 0.01) {

      # if so correct all years since 2001 (first year of buggy data)
      # using secondary forest area as buffer
      buggedYears <- intersect(2001:2015, years)
      pasture     <- setYears(x["JPN", "y2000", c("pastr", "range")], NULL)
      x["JPN", buggedYears, "secdf"]             <- x["JPN", buggedYears, "secdf"] - dimSums(pasture, dim = 3)
      x["JPN", buggedYears, c("pastr", "range")] <- x["JPN", buggedYears, c("pastr", "range")] + pasture

      # correct for negative values if secondary forest is exceeded
      secdfNegative <- (x["JPN", buggedYears, "secdf"] < 0)
      x["JPN", buggedYears, "pastr"][secdfNegative] <- x["JPN", buggedYears, "pastr"][secdfNegative] +
                                                        x["JPN", buggedYears, "secdf"][secdfNegative]
      x["JPN", buggedYears, "secdf"][secdfNegative] <- 0

      # correct potentially newly introduced negative values in rangelands
      pastrNegative <- (x["JPN", buggedYears, "pastr"] < 0)
      x["JPN", buggedYears, "range"][pastrNegative] <- x["JPN", buggedYears, "range"][pastrNegative] +
                                                        x["JPN", buggedYears, "pastr"][pastrNegative]
      x["JPN", buggedYears, "pastr"][pastrNegative] <- 0
      x["JPN", buggedYears, "range"][x["JPN", buggedYears, "range"] < 0] <- 0

    } else {
      stop("it seems the Japan bug in LUH2v2 has been removed.
           Please remove the bugfix in correct LUH2v2 before proceeding!")
    }
  }

  return(x)
}
