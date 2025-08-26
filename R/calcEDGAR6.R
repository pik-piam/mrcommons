#' calcEDGAR6
#'
#' Creates an emission inventory based on the EDGAR5 and EDGAR6 database.
#'
#' @param non_country can alternatively provide SEA and AIR emissions, which are global emissions that
#' cannot be attributed to a country
#' @param nutrient if TRUE, nitrogen is reported as N and CO2 as C
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#'
#' @author Benjamin Leon Bodirsky
calcEDGAR6 <- function(non_country = FALSE, nutrient = TRUE) { # nolint
  out <- list()
  itemnames <- NULL
  commonyears <- NULL
  pollutants <- c("n2o", "ch4", "co2_incl_short", "co2_excl_short",
                  "nh3", "no2", "bc", "co", "oc", "nmvoc", "pm10", "pm25", "so2")
  for (subtype in pollutants) {
    if (non_country %in% c("SEA", "AIR")) {
      emis <- readSource("EDGAR6", subtype = subtype, convert = FALSE)
      emis <- emis[non_country, , ]
    } else {
      emis <- readSource("EDGAR6", subtype = subtype, convert = TRUE)
    }

    emis <- complete_magpie(emis)
    out <- c(out, list(emis))
    itemnames <- sort(unique(c(getNames(emis, dim = 2), itemnames)))
    commonyears <- sort(unique(c(getYears(emis), commonyears)))
  }
  out2 <- NULL
  for (i in out) {
    tmp <- i
    tmp <- add_columns(x = tmp, addnm = setdiff(itemnames, getNames(tmp, dim = 2)), dim = 3.2)
    tmp <- add_columns(x = tmp, addnm = setdiff(commonyears, getYears(tmp)), dim = 2.1)
    if (ndim(tmp, dim = 3) == 2) {
      tmp <- add_dimension(tmp, dim = 3.3, add = "fossil_bio", nm = "non_specified")
    }
    tmp <- add_columns(x = tmp, addnm = setdiff(c("fossil", "bio", "non_specified"), getNames(tmp, dim = 3)), dim = 3.3)
    out2 <- c(out2, list(tmp))
  }

  v56 <- mbind(out2)
  v56[is.na(v56)] <- 0
  v56 <- v56 / 1000

  reformulate <- function(from, to, factor, v56) {
    getNames(v56, dim = 1) <- sub(pattern = from, replacement = to, x = getNames(v56, dim = 1))
    v56[, , to] <- v56[, , to] * factor
    return(v56)
  }
  if (nutrient) {
    v56 <- reformulate("n2o", "n2o_n", 1 / 44 * 28, v56)
    v56 <- reformulate("no2", "no2_n", 1 / 46 * 14, v56)
    v56 <- reformulate("nh3", "nh3_n", 1 / 17 * 14, v56)
    v56 <- reformulate("co2_excl_short", "co2_c", 1 / 44 * 12, v56)
  }

  return(list(x = v56,
              weight = NULL,
              unit = "Mt",
              description = "Emission inventory for EDGAR5 and EDGAR6 emissions"))
}
