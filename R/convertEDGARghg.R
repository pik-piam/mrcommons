convertEDGARghg <- function(x, subtype) {

  # rename regions
  getItems(x, dim = 1) <- sub(pattern = "ANT", replacement = "SXM",
                              x = getItems(x, dim = 1))
  getItems(x, dim = 1) <- sub(pattern = "SCG", replacement = "SRB",
                              x = getItems(x, dim = 1))

  if (subtype == "ghg_by_sector") {
    # Analog to convertCEDS: shipping and aviation data is global only
    # We want to distribute it evenly across all countries.
    # Therefore, save global data because it will be removed by toolCountryfill

    # sum all sub-sectors of int. aviation and shipping per pollutant
    air <- dimSums(x["AIR", , ], dim = 3.2, na.rm = TRUE) %>%
      add_dimension(dim = 3.2, add = "variable", nm = "Aviation")
    getItems(air, dim = 1) <- "GLO"

    sea <- dimSums(x["SEA", , ], dim = 3.2, na.rm = TRUE) %>%
      add_dimension(dim = 3.2, add = "variable", nm = "Shipping")
    getItems(sea, dim = 1) <- "GLO"

    x <- x[c("AIR", "SEA"), , invert = TRUE]

    # combine bunker data
    bunkers <- mbind(air, sea)

    # fills missing ISO countries and remove unknown ISO countries and bunkers
    x <- toolCountryFill(x, fill = 0, verbosity = 2)

    # create weight 1 for bunkers
    w <- new.magpie(getItems(x, dim = 1),
                    getItems(x, dim = 2),
                    getItems(bunkers, dim = 3),
                    fill = 1)

    # create mapping of each country to GLO
    m <- data.frame(from = getItems(x, dim = 1), to = "GLO")

    # spread global shipping and aviation data evenly across countries and
    # save it to regions of x
    x <- add_columns(x, addnm = c("Aviation", "Shipping"), dim = 3.2, fill = NA)
    x[, , getItems(bunkers, dim = 3)] <- toolAggregate(bunkers, m, weight = w)

  } else {

    # lulucf data doesn't contain AIR and SEA as it doesn't contain bunkers
    # fills missing ISO countries and remove unknown ISO countries
    x <- toolCountryFill(x, fill = 0, verbosity = 2)
  }

  return(x)
}
