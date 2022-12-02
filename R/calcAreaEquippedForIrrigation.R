#' @title calcAreaEquippedForIrrigation
#' @description Calculates the area equipped for irrigation based on LU2v2 or
#'              Siebert or Mehta data sets.
#'              For LUH2v2, it assumes, that all cropland irrigated in the last
#'              20 years at least once is equipped for irrigation.
#'
#' @param cellular    if true, dataset is returned on 0.5 degree resolution
#' @param cells       number of cells to be returned: magpiecell (59199), lpjcell (67420)
#' @param source      switch between different data sources
#' @param selectyears default on "past"
#'
#' @return List of magpie objects with results on country/cellular level,
#'         weight on country level, unit and description.
#'
#' @author Benjamin Leon Bodirsky, Kristine Karstens, Felicitas Beier
#'
#' @seealso
#' [calcLanduseInitialisation()]
#' @examples
#' \dontrun{
#' calcOutput("AreaEquippedForIrrigation", source = "LUH2v2", cellular = TRUE, aggregate = FALSE)
#' }
#' @importFrom luscale groupAggregate
#' @importFrom magpiesets findset
#'
#' @export


calcAreaEquippedForIrrigation <- function(cellular = FALSE, cells = "magpiecell",
                                          source = "LUH2v2", selectyears = "past") {

  selectyears <- sort(findset(selectyears, noset = "original"))

  if (source == "LUH2v2") {

    yearsNeeded <- as.integer(substring(selectyears, 2))
    yearsNeeded <- (yearsNeeded[1] - 20):tail(yearsNeeded, 1)

    x     <- calcOutput("LUH2v2", landuse_types = "magpie", irrigation = TRUE,
                        cellular = TRUE, cells = cells,
                        selectyears = yearsNeeded, aggregate = FALSE)[, , "irrigated"]
    x     <- dimSums(x, dim = 3)
    years <- as.numeric(substring(selectyears, 2))
    out   <- NULL

    for (year_x in years) {
      span <- (year_x - 20):year_x
      tmp  <- setYears(as.magpie(apply(X = x[, span, ], FUN = max, MARGIN = 1)), paste0("y", year_x))
      out  <- mbind(out, tmp)
    }

  } else if (source == "Siebert") {

    out   <- readSource("Siebert", convert = "onlycorrect")

    if (cells == "lpjcell") {
      stop("Old Siebert data is not available at this resolution.
           Please consider using the updated data set Mehta et al. 2022")
    }

  } else if (source == "Mehta2022") {

    out   <- readSource("Mehta2022", convert = "onlycorrect")

    if (cells == "magpiecell") {

      out <- toolCoord2Isocell(out)

    }

  } else {
    stop("Unknown source for calcAreaEquippedForIrrigation")
  }

  if (!cellular) {

    if (length(getItems(out, dim = 1)) == 67420) {
      # aggregate to iso level
      if (getSets(out)["d1.1"] == "cell") {
        out <- dimSums(x, dim = "cell")
      } else {
        out <- dimSums(out, dim = c("x", "y"))
      }
    } else {

      mapping <- toolGetMapping(name = "CountryToCellMapping.rds",
                                where = "mrcommons")

      out     <- toolAggregate(out, rel = mapping,
                               from = "celliso", to = "iso", dim = 1)
    }

    out <- toolCountryFill(out, fill = 0)
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Million ha",
              description  = "Area equipped for irrigation",
              isocountries = !cellular))
}
