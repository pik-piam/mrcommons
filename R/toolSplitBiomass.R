#' Split Biomass into modern and traditional
#'
#' We assume that below a given GDP/cap level, all biomass is traditional and
#' above a higher limit, all biomass is modern with a linear transition between
#' the limits.
#'
#' @param x MagPIE object including biomass data
#' @param gdppop MagPIE object with GDP/cap data
#' @param split character, name of item to split
#' @param into character vector of length two with the names of the split items
#' @param dim dimension of \code{x} with the item to split
#' @param limits numeric vector of length two with the corresponding GDP/cap
#'   limits. The default values used to be 10k and 15k USD/cap converted from
#'   2005 to 2017 dollars.
#' @returns MagPIE object including split biomass data
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr %>%
#' @importFrom magclass time_interpolate setNames getYears
#' @export

toolSplitBiomass <- function(x,
                             gdppop,
                             split = "biomass",
                             into = c("biotrad", "biomod"),
                             dim = 3.1,
                             limits = c(1.23E4, 1.85E4)) {

  .rename <- function(x, to) {
    getItems(x, dim)[getItems(x, dim) == split] <- to
    return(x)
  }

  r <- getItems(x, 1)
  weight <- ((gdppop[r, , ] - limits[1]) / (limits[2] - limits[1])) %>%
    collapseDim() %>%
    time_interpolate(getYears(x), extrapolation_type = "constant") %>%
    pmax(0) %>%
    pmin(1)

  items <- setdiff(getItems(x, dim), split)
  xSplit <- lapply(into, function(i) if (i %in% items) x[, , i] else 0)

  mbind(xSplit[[2]] + .rename(x[, , split] * weight,       into[2]),
        xSplit[[1]] + .rename(x[, , split] * (1 - weight), into[1]),
        x[, , setdiff(getItems(x, dim), c(split, into))])
}
