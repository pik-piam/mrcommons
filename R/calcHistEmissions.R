#' @title historical emissions per sector or mac
#' @description
#' Provides historical emissions values per economic sector or per mac sector.
#' For now it only includes European data.
#'
#' @param subtype Either "sector" or "MAC"
#' @return magpie object of historical emissions data
#' @author Renato Rodrigues
#' @examples
#' \dontrun{
#' calcOutput("HistEmissions")
#' }
#' @importFrom utils read.csv2


calcHistEmissions <- function(subtype = "sector") {

  if (subtype == "sector") {
    ### European data from Eurostat ("GHG","CO2","CH4","N2O","HFC","PFC","HFC_PFC_NSP","SF6","NF3")
    emi <- readSource(type = "Eurostat", subtype = "sectorEmi")[, seq(1998, 2017, 1), c("CO2", "CH4", "N2O")]
    getNames(emi, dim = 1) <- c("co2", "ch4", "n2o")
    description <- "Historical emissions per sector"
    gwpCH4Eurostat <- 25 # values from AR4
    gwpN2OEurostat <- 298 # values from AR4
    emi[, , "ch4"] <- emi[, , "ch4"] / gwpCH4Eurostat
    emi[, , "n2o"] <- emi[, , "n2o"] / gwpN2OEurostat
    # list of countries with Eurostat data
    cEurostat <- where(emi > 0)$true[]$regions
    cNonEU <- setdiff(getItems(emi, dim = 1), cEurostat)


    ### non-European data from CEDS
    ch4   <- readSource("CEDS", subtype = "CH4")
    co2   <- readSource("CEDS", subtype = "CO2")
    n2o   <- readSource("CEDS", subtype = "N2O")
    y <- Reduce(intersect, list(getYears(ch4),
                                getYears(co2),
                                getYears(n2o)))
    emiOt <- mbind(ch4[, y, ], co2[, y, ], n2o[, y, ]) / 1000 # kt to Mt
    if (any(!emiOt[, , "6B_Other-not-in-total"] == 0)) {
      cat("CEDS59 sector 6B_Other-not-in-total was removed although it contains data!
          Please check CEDS source files.\n")
    }
    emiOt <- emiOt[, , "6B_Other-not-in-total", invert = TRUE]
    mapCEDS59ToSec  <- read.csv2(toolGetMapping(type = "sectoral", name = "mappingCEDS59toSECTOR17.csv",
                                                returnPathOnly = TRUE),
                                 stringsAsFactors = FALSE)
    emiOt <- toolAggregate(x = emiOt, weight = NULL, dim = 3.1, rel = mapCEDS59ToSec, from = "CEDS59", to = "SECTOR")
    # add cdr-process and indirect-process
    tmp <- emiOt[, , c("bunkers-energy", "power-energy")] * 0
    getNames(tmp, dim = 1) <- c("cdr-process", "indirect-process")
    emiOt <- mbind(emiOt, tmp)
    # remove third entry "units" in data dimension
    emiOt <- collapseNames(emiOt, collapsedim = 3)
    # lower case
    map <- c(CH4 = "ch4", CO2 = "co2", N2O = "n2o")
    getNames(emiOt, dim = 2) <- map[getNames(emiOt, dim = 2)]
    # switch order
    emiOt <- dimOrder(emiOt, perm = c(2, 1))
    # split 2nd data column
    getNames(emiOt, dim = 2) <- gsub(x = getNames(emiOt, dim = 2), pattern = "-", replacement = ".")
    # add 2016 and 2017, assuming constant emissions
    emiOt <- mbind(emiOt, setYears(emiOt[, 2015, ], 2016), setYears(emiOt[, 2015, ], 2017))


    ### binding together european for european region(s) and non-european data for all others
    emi <- mbind(emi[cEurostat, , ], emiOt[cNonEU, seq(1998, 2017), ])

  } else if (subtype == "MAC") {
    ### European data from Eurostat
    emi <- readSource(type = "Eurostat", subtype = "MACCemi")[, seq(1998, 2017, 1), ]
    description <- "Historical emissions per MAC sector"
    gwpCH4Eurostat <- 25 # values from AR4
    gwpN2OEurostat <- 298 # values from AR4
    ch4Vars <- grep("ch4", getNames(emi), value = TRUE)
    n2oVars <- grep("n2o", getNames(emi), value = TRUE)
    emi[, , ch4Vars] <- emi[, , ch4Vars] / gwpCH4Eurostat
    emi[, , n2oVars] <- emi[, , n2oVars] / gwpN2OEurostat
    # list of countries with Eurostat data
    cEurostat <- where(emi > 0)$true[]$regions
    cNonEU <- setdiff(getItems(emi, dim = 1), cEurostat)

    ch4   <- readSource("CEDS", subtype = "CH4")
    co2   <- readSource("CEDS", subtype = "CO2")
    n2o   <- readSource("CEDS", subtype = "N2O")
    if (any(!ch4[, , "6B_Other-not-in-total"] == 0) ||
        any(!co2[, , "6B_Other-not-in-total"] == 0) ||
        any(!n2o[, , "6B_Other-not-in-total"] == 0)) {
      cat("CEDS59 sector 6B_Other-not-in-total was removed although it contains data!
          Please check CEDS source files.\n")
    }
    ch4 <- ch4[, , "6B_Other-not-in-total", invert = TRUE]
    co2 <- co2[, , "6B_Other-not-in-total", invert = TRUE]
    n2o <- n2o[, , "6B_Other-not-in-total", invert = TRUE]
    mapCEDS59ToMAC  <- read.csv2(toolGetMapping(type = "sectoral", name = "mappingCEDS59toMACperGas.csv",
                                                returnPathOnly = TRUE),
                                 stringsAsFactors = FALSE)
    ch4 <- toolAggregate(x = ch4, weight = NULL, dim = 3.1, rel = mapCEDS59ToMAC, from = "CEDS59", to = "MAC.ch4")
    co2 <- toolAggregate(x = co2, weight = NULL, dim = 3.1, rel = mapCEDS59ToMAC, from = "CEDS59", to = "MAC.co2")
    n2o <- toolAggregate(x = n2o, weight = NULL, dim = 3.1, rel = mapCEDS59ToMAC, from = "CEDS59", to = "MAC.n20")

    y <- Reduce(intersect, list(getYears(ch4),
                                getYears(co2),
                                getYears(n2o)))
    emiOt <- mbind(ch4[, y, ], co2[, y, ], n2o[, y, ]) / 1000 # kt to Mt
    # get rid of other categories
    emiOt <- emiOt[, , c(".CO2.kt", ".CH4.kt", ".N2O.kt"), invert = TRUE]
    # remove second and third entry "units" in data dimension
    emiOt <- collapseNames(emiOt, collapsedim = c(2, 3))
    # add 2016 and 2017, assuming constant emissions
    emiOt <- mbind(emiOt, setYears(emiOt[, 2015, ], 2016), setYears(emiOt[, 2015, ], 2017))


    ### binding together european for european region(s) and non-european data for all others
    emi <- mbind(emi[cEurostat, , ], emiOt[cNonEU, seq(1998, 2017), ])

  }

  out <- new.magpie(cells_and_regions = getItems(emi, dim = 1),
                    years = c(2000, 2005, 2010, 2015),
                    names = getNames(emi))
  out[, 2000, ] <- dimSums(emi[, c(1998, 1999, 2000, 2001, 2002), ], dim = 2) / 5
  out[, 2005, ] <- dimSums(emi[, c(2003, 2004, 2005, 2006, 2007), ], dim = 2) / 5
  out[, 2010, ] <- dimSums(emi[, c(2008, 2009, 2010, 2011, 2012), ], dim = 2) / 5
  out[, 2015, ] <- dimSums(emi[, c(2013, 2014, 2015, 2016, 2017), ], dim = 2) / 5

  # Returning emission values
  return(list(x = out, weight = NULL,
              unit = "Mt CH4/N2O/CO2 respectively",
              description = description
  ))
}
