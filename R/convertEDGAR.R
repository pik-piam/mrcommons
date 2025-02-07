convertEDGAR <- function(x, subtype) {

  if (subtype %in% c("ch4waste", "n2owaste", "co2", "CO", "NOx", "VOC", "NH3", "SO2", "PM10", "GHG")) {
    # split regional data
    # "ANT" -> "SXM", "CUW"
    # "SCG" -> "SRB", "MNE"
    m <- matrix(c(c("ANT", "ANT", "SCG", "SCG"), c("SXM", "CUW", "SRB", "MNE")), 4)
    w <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[c("SXM", "CUW", "SRB", "MNE"), 2010, ]
    xSplit <- toolAggregate(x[c("ANT", "SCG"), , ], m, weight = w)
    # delete ANT and SCG from x
    x <- x[c("ANT", "SCG"), , invert = TRUE]
    x <- mbind(x, xSplit)
  } else if (subtype == "ch4_history") {
    # split regional data
    # "SCG" -> "SRB", "MNE"
    m <- matrix(c(c("SCG", "SCG"), c("SRB", "MNE")), 2)
    w <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[c("SRB", "MNE"), 2005, ]
    xSplit <- toolAggregate(x["SCG", , ], m, weight = w)
    # delete SCG from x
    x <- x[c("SCG"), , invert = TRUE]
    x <- mbind(x, xSplit)
    x[is.na(x)] <- 0
    x <- toolISOhistorical(x)
  } else if (subtype == "HFC") {
    # split regional data
    # "SCG" -> "SRB", "MNE"
    m <- matrix(c(c("SCG", "SCG"), c("SRB", "MNE")), 2)
    w <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[c("SRB", "MNE"), 2005, ]
    xSplit <- toolAggregate(x["SCG", , ], m, weight = w)
    # delete SCG from x
    x <- x[c("SCG"), , invert = TRUE]
    x <- mbind(x, xSplit)
  }


  if (subtype %in% c("ch4waste", "n2owaste", "co2", "ch4_history", "CO", "NOx", "VOC", "NH3", "SO2", "PM10")) {
    # In the EDGAR data source shipping and aviation emissions are reported in global values in extra regions called AIR
    # and SEA, put international shipping (SEA) and international aviation emissions (AIR) in a separate dimension
    xSeaAir <- new.magpie("GLO", getYears(x), c("SEA", "AIR"))
    xSeaAir[, , "SEA"] <- x["SEA", , "TOTAL"]
    xSeaAir[, , "AIR"] <- x["AIR", , "TOTAL"]
    # delete SEA and AIR from regional dimension
    # and allocate to all countries
    x <- x[c("SEA", "AIR"), , invert = TRUE]
    m <- matrix(c(getItems(x, dim = 1), rep("GLO", length(getItems(x, dim = 1)))), length(getItems(x, dim = 1)))
    w <- new.magpie(getItems(x, dim = 1), getYears(x), fill = 1)      # FIXME use GDP as weight # nolint
    xAdd <- toolAggregate(xSeaAir, m, weight = w)
    # Allocate to existing but empty variables in x
    x[, , "1C1"] <- xAdd[, , "AIR"]
    x[, , "1C2"] <- xAdd[, , "SEA"]

  }

  # fill all missing countries with 0
  x <- toolCountryFill(x, fill = 0, verbosity = 2)
  return(x)
}
