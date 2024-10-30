#' @title readNitrogenBoundariesGridded
#' @description Read the grid-level regional nitrogen boundary datasets from Schulte-Uebbing et al. (2022).
#' For the moment, this only uses the critical N surplus in all agricultural land (arable + intensively
#' managed grassland) in view of all thresholds simultaneously, measured in kg N per ha per yr.
#' @author Michael S. Crawford
#'
#' @return A MAgPIE object with the critical nitrogen surplus in kg N per ha per yr in 2010
#'
#' @seealso [readSource()]
#' @examples \dontrun{
#'  a <- readSource("NitrogenBoundariesGridded")
#' }

readNitrogenBoundariesGridded <- function() {

  longitude <- seq(-179.750, 179.750, by = 0.50)
  latitude <- seq(89.750, -89.750, by = -0.50)

  d <- read.table("nsur_crit_mi_all_ph.asc",
                  col.names = longitude,
                  header = FALSE, skip = 6, check.names = FALSE)

  d <- as.data.frame(d) %>%
    dplyr::mutate(lat = latitude) %>%
    tidyr::pivot_longer(cols = -"lat", names_to = "lon", values_to = "critNitrSurplus") %>%
    dplyr::mutate(lon = as.numeric(.data[["lon"]]))

  d$critNitrSurplus <- replace(d$critNitrSurplus, d$critNitrSurplus == -9999, NA)

  # Filter to use MAgPIE's celliso format
  mapping <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mstools") %>% # nolint: object_usage_linter.
    dplyr::select("celliso", "lon", "lat")

  d <- dplyr::inner_join(d, mapping, by = c("lat", "lon")) %>%
    dplyr::mutate(year = "y2010") %>%
    dplyr::select("celliso", "year", "critNitrSurplus")

  mag <- as.magpie(d, tidy = TRUE, filter = FALSE)

  return(mag)

}
