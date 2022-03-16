#' @title convertLassaletta2014
#' @description converts the dataset of
#' Lassaletta, L., G. Billen, B. Grizzetti, J. Angalde, and J. Garnier. 2014. 
#' 50 Year Trends in Nitrogen Use Efficiency of World Cropping Systems: The Relationship between Yield and Nitrogen Input to Cropland.
#' Environmental Research Letters.
#' into a dataset including all countries. Replacing Soviet Union by Russia and Yugoslavia by Serbia without detailed disaggregation.
#' @param x data object that gets provided by wrapper function readSource
#' @param subtype budget provides the nr cropland budgets, fert_to_cropland the sahre of inorganic fertilizers being applied to croplands
#' @return Magpie object with results on country level.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [readLassaletta2014()],
#' [readSource()]
#' @examples
#' 
#' \dontrun{ 
#' readSource("Lassaletta2014",convert=TRUE)
#' }
#' @importFrom madrat toolAggregate toolCountryFill
#' @importFrom magclass add_columns setCells getYears
convertLassaletta2014 <- function(x, subtype) {
  if (subtype == "budget") {
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
    gdpXFS2012 <- c(SSD = 15084.7120285034, SDN = 132693.678886486)
    gdpCSK1993 <- c(CZE = 155945.557514784, SVK = 52390.8486317272)
    weight <- c(gdpSUN1992, gdpYUG1992, gdpXET1996, gdpXFS2012, gdpCSK1993)

    mapping <- rbind(data.frame(from = "SUN", to = names(gdpSUN1992)),
                     data.frame(from = "YUG", to = names(gdpYUG1992)),
                     data.frame(from = "XET", to = names(gdpXET1996)),
                     data.frame(from = "XFS", to = names(gdpXFS2012)),
                     data.frame(from = "CSK", to = names(gdpCSK1993)))

    newCountries <- names(weight)
    x <- add_columns(x, newCountries, dim = 1.1)
    historicalCountries <- unique(mapping$from)

    x[newCountries, , ] <- toolAggregate(x[historicalCountries, , ], mapping, as.magpie(weight))

    x <- x[historicalCountries, , , invert = TRUE]
    x <- toolCountryFill(x, fill = 0)
  } else if (subtype == "fert_to_cropland") {
    x <- x / 100
    x2 <- toolCountryFill(x, fill = 0)
    mapping <- read.csv2(system.file("extdata", "ISOhistorical.csv", 
                                     package = "madrat"), stringsAsFactors = FALSE)

    x2[c("SRB","MNE","SVN","HRV","MKD","BIH"),,]<-setCells(x["YUG",,],"GLO")
    x2[mapping$toISO[which(mapping$fromISO =="SUN")],,]<-setCells(x["SUN",,],"GLO")
    x2[mapping$toISO[which(mapping$fromISO =="CSK")],,]<-setCells(x["CSK",,],"GLO")

    x<-x2
  }

  return(x)
}
