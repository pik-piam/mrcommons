#' @title convertWCDE
#'
#' @description It fills the missing countries of the output of readWCDE with
#' the population-weighted average structure of one or more countries with
#' similar characteristics, scaled with the population of the missing country.
#' For years before the population data starts (the historical reconstruction
#' of subtype "epop_v2" goes back to 1950), the earliest available population
#' estimate is held constant; this only affects small countries and territories.
#' @param x magpie object provided by the read function
#' @param subtype "epop_v3" or "epop_v2" (only needed so that readSource can
#' resolve the default subtype consistently for read and convert; the country
#' filling is identical for both subtypes)
#'
#' @seealso
#' [readWCDE()], [convertLutz2014()]

convertWCDE <- function(x, subtype) {

  x <- toolCountryFill(x, fill = NA)

  # donor countries used to fill countries missing in the source data,
  # following convertLutz2014. TWN is kept as a safety net only: the WCDE
  # datasets contain real data for Taiwan (mapped in readWCDE), so it is
  # normally not among the missing countries.
  donors <- list(
    ABW = c("PRI"),
    AIA = c("PRI"),
    ALA = c("ISL", "EST"),
    AND = c("FRA", "ESP"),
    ASM = c("NCL", "WSM"),
    ATA = c("ISL", "EST"),
    ATF = c("ISL", "EST"),
    ATG = c("PRI"),
    BES = c("PRI"),
    BLM = c("PRI"),
    BMU = c("PRI"),
    BVT = c("ISL", "EST"),
    CCK = c("PRI"),
    COK = c("NCL", "WSM"),
    CUW = c("PRI"),
    CXR = c("NCL", "WSM"),
    CYM = c("PRI"),
    DMA = c("PRI"),
    ERI = c("ETH", "DJI"),
    ESH = c("MRT", "MLI"),
    FLK = c("ISL", "EST"),
    FRO = c("ISL", "EST"),
    GGY = c("ISL", "EST"),
    GIB = c("GBR", "ESP"),
    GRL = c("ISL", "EST"),
    HMD = c("ISL", "EST"),
    IMN = c("ISL", "EST"),
    IOT = c("NCL", "WSM"),
    JEY = c("ISL", "EST"),
    KIR = c("NCL", "WSM"),
    KNA = c("PRI"),
    LIE = c("CHE", "LUX"),
    MAF = c("PRI"),
    MCO = c("CHE", "LUX"),
    MHL = c("NCL", "WSM"),
    MNP = c("NCL", "WSM"),
    MSR = c("PRI"),
    NFK = c("NCL", "WSM"),
    NIU = c("NCL", "WSM"),
    NRU = c("NCL", "WSM"),
    PCN = c("NCL", "WSM"),
    PLW = c("NCL", "WSM"),
    SGS = c("ISL", "EST"),
    SHN = c("ISL", "EST"),
    SJM = c("ISL", "EST"),
    SMR = c("CHE", "LUX"),
    SPM = c("ISL", "EST"),
    SSD = c("TCD", "SDN"),
    SXM = c("PRI"),
    SYC = c("MUS", "MDV"),
    TCA = c("PRI"),
    TKL = c("NCL", "WSM"),
    TUV = c("NCL", "WSM"),
    TWN = c("KOR", "HKG"),
    UMI = c("NCL", "WSM"),
    VAT = c("CHE", "LUX"),
    VGB = c("PRI"),
    WLF = c("NCL", "WSM")
  )

  popWdi <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)

  # population of a country for all years of x; years outside the population
  # data are filled with the closest available estimate
  getPopulation <- function(country, years) {
    pop <- new.magpie(cells_and_regions = country, years = years, fill = NA)
    common <- intersect(years, getYears(popWdi))
    pop[, common, ] <- popWdi[country, common, ]
    yearsNumeric <- getYears(pop, as.integer = TRUE)
    popYears <- getYears(popWdi, as.integer = TRUE)
    early <- yearsNumeric < min(popYears)
    late <- yearsNumeric > max(popYears)
    if (any(early)) {
      pop[, early, ] <- setYears(popWdi[country, min(popYears), ], NULL)
    }
    if (any(late)) {
      pop[, late, ] <- setYears(popWdi[country, max(popYears), ], NULL)
    }
    return(pop)
  }

  fillCountryByAverageOfRegion <- function(x, country, region) {
    vcat(2, paste0("interpolating country: ", country))
    values <- x[region, , ]
    population <- getPopulation(country, getYears(values))
    average <- dimSums(values, dim = 1) / dimSums(values[, , "Total"][, , "Both"][, , "All"], dim = 1)
    x[country, , ] <- setCells(average, "GLO") * population
    return(x)
  }

  missing <- getItems(x, dim = 1)[apply(is.na(x), 1, all)]
  for (country in missing) {
    if (!country %in% names(donors)) {
      stop("No donor countries defined in convertWCDE to fill missing country ", country)
    }
    x <- fillCountryByAverageOfRegion(x, country = country, region = donors[[country]])
  }

  # The datasets contain structural NAs that are kept as NA, as in
  # convertLutz2014:
  #  - age-education combinations that do not exist (e.g. age 0-4 with a
  #    university degree),
  #  - in "epop_v2", the post-secondary split (Short Post Secondary, Bachelor,
  #    Master and higher), which is only reported from V3 onwards,
  #  - in "epop_v2", the historical reconstruction (before 2015) is missing for
  #    a handful of countries; their early years are filled at the aggregation
  #    stage in calcDemography.
  # We only assert that no country is entirely missing (which would indicate a
  # missing donor definition above).
  totals <- collapseNames(x[, , "Total"][, , "Both"][, , "All"])
  emptyCountries <- getItems(totals, dim = 1)[apply(is.na(totals), 1, all)]
  if (length(emptyCountries) > 0) {
    stop("No population data for countries: ", paste(emptyCountries, collapse = ", "))
  }
  incomplete <- getItems(totals, dim = 1)[apply(is.na(totals), 1, any)]
  if (length(incomplete) > 0) {
    vcat(2, paste0("Countries without complete history (early years missing in source): ",
                   paste(incomplete, collapse = ", ")))
  }

  return(x)
}
