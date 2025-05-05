#' Convert JRC IDEES data
#'
#' Missing data for EU-28 countries is added, by distributing the difference of
#' `EU28` and the sum of country-values based on countries share in EU-28 GDP.
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readJRC_IDEES()`].
#' @param subtype character, subtype of [`readJRC_IDEES()`]
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Michaja Pehl, Robin Hasse
#'
#' @seealso [`readJRC_IDEES()`]
#'
#' @importFrom dplyr %>% distinct filter full_join group_by left_join mutate
#'                  pull select
#' @importFrom madrat getISOlist toolCountry2isocode toolCountryFill
#' @importFrom magclass as.data.frame as.magpie getNames getNames<-
#'   add_dimension
#' @importFrom quitte add_countrycode_ character.data.frame
#' @importFrom rlang sym syms
#' @importFrom tibble as_tibble
#' @importFrom tidyr complete nesting
#'
#' @export
convertJRC_IDEES <- function(x, subtype) { # nolint

  # FUNCTIONS ------------------------------------------------------------------

  unitAsDim <- function(x) {
    oldNames <- getNames(x)
    newNames <- sub("^([^.-]+)-([^.-]+)-([^.]+)\\.(.+)$",
                    "\\1-\\2-\\3.\\4.\\2",
                    oldNames)
    x <- add_dimension(x, dim = 3.3, add = "unit")
    getNames(x) <- newNames
    return(x)
  }



  fixRegions <- function(x) {
    regions <- getItems(x, 1)
    regions[regions == "EL"] <- "Greece"
    getItems(x, 1) <- toolCountry2isocode(regions)
    x <- toolCountryFill(x, verbosity = 2)
    return(x)
  }



  # CONVERT --------------------------------------------------------------------

  ## 2021 data ====

  if (endsWith(subtype, "_2021")) {
    x <- unitAsDim(x)
    x <- fixRegions(x)
    return(x)
  }


  ## 2015 data ====

  x %>%
    # clean madrat data
    as.data.frame() %>%
    as_tibble() %>%
    select("region" = "Data1", "variable" = "Data2", "unit" = "Data3",
           "year" = "Year", "value" = "Value") %>%
    character.data.frame() %>%
    mutate(year = as.integer(!!sym("year"))) %>%
    # convert Eurostat to ISO3C contry codes
    add_countrycode_(origin = c("region" = "eurostat"),
                     destination = "iso3c",
                     warn = FALSE, na.rm = TRUE) %>%
    select(-"region") %>%
    # extend to all countries
    complete(nesting(!!!syms(c("variable", "unit", "year"))),
             iso3c = setNames(getISOlist(), NULL)) %>%
    as.magpie(tidy = TRUE) %>%
    return()
}
