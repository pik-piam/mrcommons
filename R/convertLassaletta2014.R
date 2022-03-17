#' @title convertLassaletta2014
#' @description converts the following dataset into a dataset including all countries:
#' Lassaletta, L., G. Billen, B. Grizzetti, J. Angalde, and J. Garnier. 2014.
#' 50 Year Trends in Nitrogen Use Efficiency of World Cropping Systems:
#' The Relationship between Yield and Nitrogen Input to Cropland.
#' Environmental Research Letters.
#' @param x data object that gets provided by wrapper function readSource
#' @param subtype budget: provides the nr cropland budgets,
#' fert_to_cropland: the share of inorganic fertilizers being applied to croplands
#' @return Magpie object with results on country level.
#' @examples
#' \dontrun{
#' readSource("Lassaletta2014", convert = TRUE)
#' }
#' @author Benjamin Leon Bodirsky, Pascal Führlich
#' @seealso
#' [readLassaletta2014()],
#' [readSource()]
#' @importFrom madrat toolAggregate toolCountryFill
#' @importFrom magclass add_columns getItems as.magpie
convertLassaletta2014 <- function(x, subtype) {
  if (subtype == "fert_to_cropland") {
    x <- x / 100
    stopifnot(all(x[, 1961, , ] == 0))
    x <- x[, 1961, , invert = TRUE]
  }

  # data contains data for historical countries after they no longer existed, e.g. SUN/Soviet Union in 2009
  # thus we use toolAggregate instead of toolISOhistorical

  # gdp data as disaggregation weights
  gdpSUN1992 <- c(ARM = 6925.15574116062, AZE = 40493.3241099536, BLR = 65373.4701813026,
                  EST = 11825.2182697375, GEO = 16426.4870578943, KAZ = 156466.520767634,
                  KGZ = 12176.9265194257, LVA = 17718.372396376, LTU = 28582.5097141384,
                  MDA = 11557.9931005557, RUS = 1521682.34169888, TJK = 12640.3332440118,
                  TKM = 22306.6440226757, UKR = 430459.266071488, UZB = 62046.120156688)
  gdpYUG1992 <- c(SVN = 29042.9637630123, HRV = 41524.1401730024, MKD = 13937.6578300527,
                  BIH = 4225.43827357223, SRB = 53038.6020554997, MNE = 4460.02417845057)
  gdpXET1996 <- c(ETH = 32238.1907959791, ERI = 5643.24672595384)
  gdpXSD2012 <- c(SSD = 15084.7120285034, SDN = 132693.678886486)
  gdpCSK1993 <- c(CZE = 155945.557514784, SVK = 52390.8486317272)
  weight <- c(gdpSUN1992, gdpYUG1992, gdpXET1996, gdpXSD2012, gdpCSK1993)

  mapping <- rbind(data.frame(from = "SUN", to = names(gdpSUN1992)),
                   data.frame(from = "YUG", to = names(gdpYUG1992)),
                   data.frame(from = "XET", to = names(gdpXET1996)),
                   data.frame(from = "XSD", to = names(gdpXSD2012)),
                   data.frame(from = "CSK", to = names(gdpCSK1993)))

  historicalCountries <- unique(mapping$from)
  newCountries <- names(weight)

  stopifnot(all(x[intersect(newCountries, getItems(x, dim = 1.1)), , ] == 0))
  x <- add_columns(x, setdiff(newCountries, getItems(x, dim = 1.1)), dim = 1.1)

  x[newCountries, , ] <- toolAggregate(x[historicalCountries, , ], mapping, as.magpie(weight))

  x <- x[historicalCountries, , , invert = TRUE]
  x <- toolCountryFill(x, fill = 0)
  return(x)
}
