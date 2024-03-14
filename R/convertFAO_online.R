#' Convert FAO data
#'
#' Converts FAO data to fit to the common country list and removes or converts
#' relative values where possible. Yields (Hg/ha) are for instance removed
#' since they can later easily be calculated from production and area but might
#' be problematic in the spatial aggregation. Per capita demand values are
#' transformed into absolute values using population estimates from the
#' calcPopulationPast function.
#'
#' Update 23-Jan-2017 - Added FAO Forestry production and trade data (Abhi)
#'
#' @param x MAgPIE object containing original values
#' @param subtype The FAO file type, e.g.: CBCrop
#' @return Data as MAgPIE object with common country list
#' @author Ulrich Kreidenweis, Abhijeet Mishra, Mishko Stevanovic, David Klein, Daivd Chen, Edna Molina Bacca
#' @seealso [readFAO()], [readSource()],
#' @examples
#' \dontrun{
#' a <- readSource("FAO_online", "Crop", convert = TRUE)
#' }
#' @importFrom magclass magpiesort dimExists getItems
#' @importFrom GDPuc convertGDP
#'

## check why LivePrim has such strange Units such as (0_1Gr/An) and "Yield_(Hg)"

convertFAO_online <- function(x, subtype) { # nolint: cyclocomp_linter, object_name_linter.

  # ---- Settings ----

  ## datasets that have only absolute values
  absolute <- c("CBCrop", "CBLive", "CropProc", "Fertilizer", "Land", "LiveHead",
                "LiveProc", "Pop", "ValueOfProd", "ForestProdTrade", "Fbs", "FbsHistoric",
                "FertilizerProducts", "FertilizerNutrients", "Trade", "TradeMatrix")

  ## datasets that contain relative values that can be deleted because they can
  ## be calculated again at a later point in time
  ## and the dimensions that can be deleted
  relativeDelete <- list()
  relativeDelete[["Crop"]] <- c("Yield_(hg/ha)", "Yield_(Hg/Ha)")
  relativeDelete[["Fodder"]] <- "Yield_(Hg/Ha)"
  relativeDelete[["LivePrim"]] <- c("Yield_Carcass_Weight_(Hg/An)",
                                    "Yield_(100Mg/An)",
                                    "Yield_Carcass_Weight_(0_1Gr/An)",
                                    "Yield_(Hg/An)",
                                    "Yield_(Hg)",
                                    "Yield_(100mg/An)",               # new FAO data
                                    "Yield_(hg/An)",                  # new FAO data
                                    "Yield_Carcass_Weight_(hg/An)",   # new FAO data
                                    "Yield_Carcass_Weight_(0_1g/An)", # new FAO data
                                    "Yield_(hg)")                     # new FAO data

  # Relative and unused datasets for the Capital Stock database
  relativeDelete[["CapitalStock"]] <-
    c(paste0("22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).",
             c("Value_Local_Currency_(millions)",
               "Value_Local_Currency_2015_prices_(millions)",
               "Value_USD_(millions)",
               "Share_of_Value_Added_Local_Currency_(percentage)",
               "Share_of_Value_Added_Local_Currency_2015_prices_(percentage)",
               "Share_of_Value_Added_USD_(percentage)",
               "Agriculture_orientation_index_Local_Currency_(index)",
               "Agriculture_orientation_index_Local_Currency_2015_prices_(index)",
               "Agriculture_orientation_index_USD_(index)",
               "Agriculture_orientation_index_USD_2015_prices_(index)",
               "Share_of_Gross_Fixed_Capital_Formation_USD_(percentage)",
               "Share_of_Gross_Fixed_Capital_Formation_(percentage)",
               "Share_of_Gross_Fixed_Capital_Formation_2015_prices_(percentage)")),
      paste0("22031|Consumption of Fixed Capital (Agriculture, Forestry and Fishing).",
             c("Value_Local_Currency_(millions)",
               "Value_Local_Currency_2015_prices_(millions)",
               "Value_USD_(millions)")),
      paste0("22034|Net Capital Stocks (Agriculture, Forestry and Fishing).",
             c("Value_Local_Currency_(millions)",
               "Value_Local_Currency_2015_prices_(millions)",
               "Value_USD_(millions)")),
      "22033|Gross Capital Stocks (Agriculture, Forestry and Fishing).Value_Local_Currency_(millions)",
      paste0("22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).",
             c("Share_of_Value_Added_USD_2015_prices_(percentage)",
               "Share_of_Gross_Fixed_Capital_Formation_USD_2015_prices_(percentage)",
               "Value_Local_Currency_(millions)")))

  if (subtype == "ValueShares") {
    stop(paste("Too many missing countries in Value Shares dataset to convert.",
               "Rather use as validation. Don't forget to currency convert manually."))
  }

  # select elements only if unit (dim=3.2) exists in x (otherwise magclass would complain when trying to remove
  # non-existent elements with invert=TRUE). For capital stocks selects the complete name. The dot in the original
  # dataset causes errors.
  if ((subtype %in% names(relativeDelete)) && subtype != "CapitalStock") {
    relativeDelete <- relativeDelete[[subtype]][relativeDelete[[subtype]] %in% getItems(x, dim = 3.2)]
  } else if (subtype == "CapitalStock") {
    relativeDelete <- relativeDelete[[subtype]][relativeDelete[[subtype]] %in% getItems(x, dim = 3)]
  } else {
    relativeDelete <- NULL
  }

  if (identical(relativeDelete, character(0))) {
    stop("For this subtype (", subtype, ") units are listed in 'convertFAO' whose entries should be deleted from the ",
         "data, but none of the specified units could be found in the data.")
  }

  ## datasets that contain relative values: and define these dimensions
  relative <- list()
  relative[["FSCrop"]] <- c("food_supply_kg/cap/yr",
                            "food_supply_g/cap/day",
                            "food_supply_kcal/cap/day",
                            "protein_supply_g/cap/day",
                            "fat_supply_g/cap/day")

  relative[["FSLive"]] <- c("food_supply_kg/cap/yr",
                            "food_supply_g/cap/day",
                            "food_supply_kcal/cap/day",
                            "protein_supply_g/cap/day",
                            "fat_supply_g/cap/day")

  # ---- Section for country specific treatment ----

  ## data for Eritrea ERI and South Sudan SSD added with 0 if not existing after the split
  ## to make toolISOhistorical work
  if (any(getItems(x, dim = 1.1) == "XET") &&
        any(getItems(x, dim = 1.1) == "ETH") &&
        !any(getItems(x, dim = 1.1) == "ERI")) {
    xERI <- x["ETH", , ]
    xERI[, , ] <- 0
    getItems(xERI, dim = 1) <- "ERI"
    x <- magpiesort(mbind(x, xERI))
  }

  if (any(getItems(x, dim = 1.1) == "XSD") &&
        any(getItems(x, dim = 1.1) == "SDN") &&
        !any(getItems(x, dim = 1.1) == "SSD")) {
    xSSD <- x["SDN", , ]
    xSSD[, , ] <- 0
    getItems(xSSD, dim = 1) <- "SSD"
    x <- magpiesort(mbind(x, xSSD))
  }


  ## add additional mappings
  additionalMapping <- list()

  # Eritrea ERI and Ethiopia ETH
  if (all(c("XET", "ETH", "ERI") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping, list(c("XET", "ETH", "y1992"), c("XET", "ERI", "y1992")))
  }

  # Belgium-Luxemburg
  if (all(c("XBL", "BEL", "LUX") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping, list(c("XBL", "BEL", "y1999"), c("XBL", "LUX", "y1999")))
  } else if (("XBL" %in% getItems(x, dim = 1.1)) && !("BEL" %in% getItems(x, dim = 1.1))) {
    getCells(x)[getItems(x, dim = 1.1) == "XBL"] <- "BEL"
  }

  # Sudan (former) to Sudan and Southern Sudan. If non of the latter two is in the data make Sudan (former) to Sudan
  if (all(c("XSD", "SSD", "SDN") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping, list(c("XSD", "SSD", "y2011"), c("XSD", "SDN", "y2011")))
  } else if ("XSD" %in% getItems(x, dim = 1.1) && !any(c("SSD", "SDN") %in% getItems(x, dim = 1.1))) {
    getCells(x)[getItems(x, dim = 1.1) == "XSD"] <- "SDN"
  }

  ## if XCN exists, replace CHN with XCN.
  if ("XCN" %in% getItems(x, dim = 1.1)) {
    if ("CHN" %in% getItems(x, dim = 1.1)) x <- x["CHN", , , invert = TRUE]
    getItems(x, dim = 1)[getItems(x, dim = 1) == "XCN"] <- "CHN"
  }

  ## data for the Netherlands Antilles is currently removed because currently no
  ## information for its successors SXM, CUW, ABW is available as input for toolISOhistorical
  if (any(getItems(x, dim = 1.1) == "ANT")) {
    x <- x["ANT", , , invert = TRUE]
  }

  ## data for PCI split up into:
  # Marshall Islands (MH, MHL, 584)
  # Micronesia, Federated States of (FM, FSM, 583)
  # Northern Mariana Islands (MP, MNP, 580)
  # Palau (PW, PLW, 585)
  if (all(c("PCI", "MHL", "FSM", "MNP", "PLW") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping, list(c("PCI", "MHL", "y1991"),
                                                        c("PCI", "FSM", "y1991"),
                                                        c("PCI", "MNP", "y1991"),
                                                        c("PCI", "PLW", "y1991")))
  } else if ("PCI" %in% getItems(x, dim = 1.1)) {
    x <- x["PCI", , invert = TRUE]
  }


  ### For certain subtypes: if some of the follow up states of the Soviet Union (SUN), Yugoslavia (YUG), Serbia and
  # Montenegro (SCG) are missing add them with values of 0
  if (subtype %in% c("EmisAgRiceCult", "Fertilizer", "FertilizerNutrients", "EmisAgCultOrgSoil", "EmisLuCrop",
                     "EmisLuGrass", "EmisAgSynthFerti")) {
    isoHistorical <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"),
                               stringsAsFactors = FALSE)
    former <- isoHistorical[isoHistorical$fromISO %in% c("SUN", "YUG", "SCG"), "toISO"]
    missing <- former[!former %in% getItems(x, dim = 1.1)]
    x2 <- new.magpie(cells_and_regions = missing, years = getYears(x), names = getNames(x))
    x2[, getYears(x2)[getYears(x2, as.integer = TRUE) >= 1992], ] <- 0
    x <- mbind(x, x2)
  }

  # ---- Treatment of absolute or relative values ----

  if (any(subtype == absolute)) {
    x[is.na(x)] <- 0
    if (subtype != "Fbs") {
      x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additionalMapping)
    }
    x <- toolCountryFill(x, fill = 0, verbosity = 2)
    if (any(grepl(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]]))) {
      warning("The following elements could be relative: \n",
              paste(grep(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]], value = TRUE), collapse = " "),
              "\n", "and would need a different treatment of NAs in convertFAO")
    }

  } else if (!is.null(relativeDelete)) {
    x[is.na(x)] <- 0
    x <- x[, , relativeDelete, invert = TRUE]
    if (subtype != "CapitalStock") {
      # Capital Stock available starting from 1995 (no need for transitions)
      x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additionalMapping)
    }
    x <- toolCountryFill(x, fill = 0, verbosity = 2)
    if (subtype != "CapitalStock") {
      if (any(grepl(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]]))) {
        warning("The following elements could be relative: \n",
                paste(grep(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]], value = TRUE), collapse = " "),
                "\n", "and would need a different treatment of NAs in convertFAO")
      }
    }

  } else if (any(subtype == c("FSCrop", "FSLive"))) {

    xabs <- x[, , relative[[subtype]], invert = TRUE]
    xrel <- x[, , relative[[subtype]], invert = FALSE]

    # handling of relative values
    # replaced toolISOhistorical by the following approach for disaggregation
    mapping <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"), stringsAsFactors = FALSE)
    for (elem in additionalMapping) {
      mapping <- rbind(mapping, elem)
    }

    .adoptAggregatedAverage <- function(country, data, mapping) {
      if (length(country) > 1) stop("only one transition per function call")

      toISO <- mapping$toISO[mapping$fromISO == country]
      lastyear <- unique(mapping$lastYear[mapping$fromISO == country])

      if (length(lastyear) > 1) stop("strange transition mapping")

      allyears <- getYears(data, as.integer = TRUE)
      years <- allyears[allyears <= as.integer(substring(lastyear, 2, 5))]
      data[toISO, years, ] <- magclass::colSums(data[country, years])
      data <- data[country, , , invert = TRUE]
      return(data)
    }

    xrel <- .adoptAggregatedAverage(country = "SUN", data = xrel, mapping = mapping)
    xrel <- .adoptAggregatedAverage(country = "YUG", data = xrel, mapping = mapping)
    xrel <- .adoptAggregatedAverage(country = "CSK", data = xrel, mapping = mapping)
    xrel <- .adoptAggregatedAverage(country = "XET", data = xrel, mapping = mapping)
    xrel <- .adoptAggregatedAverage(country = "XBL", data = xrel, mapping = mapping)
    xrel <- .adoptAggregatedAverage(country = "SCG", data = xrel, mapping = mapping)
    xrel <- .adoptAggregatedAverage(country = "XSD", data = xrel, mapping = mapping)

    # transforming relative values into absolute values
    pop <- calcOutput("PopulationPast", aggregate = FALSE)
    xrel <- toolCountryFill(xrel, fill = 0, verbosity = 2)
    commonyears <- intersect(getYears(pop), getYears(x))
    xrelpop <- collapseNames(complete_magpie(pop[, commonyears, ]) * complete_magpie(xrel[, commonyears, ]))
    xrelpop <- xrelpop[, , c("food_supply_kcal/cap/day", "protein_supply_g/cap/day", "fat_supply_g/cap/day")] * 365
    getNames(xrelpop, dim = 2) <- c("food_supply_kcal", "protein_supply", "fat_supply")
    xrelpop[is.na(xrelpop)] <- 0

    # absolute values
    xabs[is.na(xabs)] <- 0
    xabs[xabs < 0] <- 0
    xabs <- toolISOhistorical(xabs, overwrite = TRUE, additional_mapping = additionalMapping)
    xabs <- toolCountryFill(xabs, fill = 0, verbosity = 2)

    x <- mbind(xabs, xrelpop)
    x <- complete_magpie(x)
    x <- toolCountryFill(x, fill = 0, verbosity = 2)
    if (any(grepl(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]]))) {
      warning("The following elements could be relative: \n",
              paste(grep(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]], value = TRUE), collapse = " "),
              "\n", "and would need a different treatment of NAs in convertFAO")
    }

    # automatically delete the "Implied emissions factor XXX" dimension for Emission datasets
  } else if (substring(subtype, 1, 6) == "EmisAg" || substring(subtype, 1, 6) == "EmisLu") {
    if (any(grepl("Implied_emission_factor", getItems(x, dim = 3.2)))) {
      x <- x[, , "Implied_emission_factor", pmatch = TRUE, invert = TRUE]
    }
    x[is.na(x)] <- 0
    x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additionalMapping)
    x <- toolCountryFill(x, fill = 0, verbosity = 2)

    # Producer Prices Annual
  } else if (subtype %in% c("PricesProducerAnnual", "PricesProducerAnnualLCU")) {
    # FAO changed the unit. Look for all possible names and select only existing ones from the magpie object
    possibleNames <- list(PricesProducerAnnual = c("Producer_Price_(US_$_tonne)_(USD)",
                                                   "Producer_Price_(USD_tonne)_(USD)"),
                          PricesProducerAnnualLCU = c("Producer_Price_(Standard_local_Currency_tonne)_(SLC)",
                                                      "Producer_Price_(SLC_tonne)_(SLC)"))
    possibleNames <- toolSubtypeSelect(subtype, possibleNames)
    x <- collapseNames(x[, , possibleNames[possibleNames %in% getItems(x, dim = 3.2)]])
    ## Serbia and Montenegro split
    if (all(c("SCG", "SRB") %in% getItems(x, dim = 1.1)) && !"MNE" %in% getItems(x, dim = 1.1)) {
      mne <- x["SRB", , ]
      dimnames(mne)[[1]] <- "MNE"
      x <- mbind(x, mne)
    }
    ## Adjust prices of live animal weight to the carcass weight
    mapping <- toolGetMapping("FAO_livestock_carcass_price_factor.csv", type = "sectoral", where = "mrcommons")
    for (item in mapping$FAO_carcass) {
      itemn <- gsub("([0-9]+).*$", "\\1", item)
      litem <- mapping$FAO_live_weigth[grep(item, mapping$FAO_carcass)]
      litemn <- gsub("([0-9]+).*$", "\\1", litem)
      countries <- unique(rownames(which(!is.na(x[, , itemn, pmatch = TRUE]), arr.ind = TRUE)))
      countries <- setdiff(getItems(x, dim = 1.1), countries)
      x[countries, , itemn, pmatch = TRUE] <- x[countries, , litemn, pmatch = TRUE] /
        mapping$Price_factor[grep(item, mapping$FAO_carcass)]
    }
    x[is.na(x)] <- 0
    x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additionalMapping)
    x <- toolCountryFill(x, fill = 0, verbosity = 2)

    if (subtype == "PricesProducerAnnual") {
      x <- convertGDP(x, unit_in = "current US$MER",
                      unit_out = "constant 2005 US$MER",
                      replace_NAs = "no_conversion")

    } else if (subtype == "PricesProducerAnnualLCU") {
      x <- convertGDP(x, unit_in = "current LCU",
                      unit_out = "constant 2005 LCU",
                      replace_NAs = "no_conversion")
    }

  } else {
    cat("Specify in convertFAO whether dataset contains absolute or relative values!")
  }

  if (subtype == "ValueOfProd") {
    x2 <- x[, , "Gross_Production_Value_(current_thousand_US$)_(1000_US$)"]
    x2 <- convertGDP(x2, unit_in = "current US$MER",
                     unit_out = "constant 2005 US$MER",
                     replace_NAs = "no_conversion")
    getNames(x2, dim = 2) <- "Gross_Production_Value_(USDMER05)_(1000_US$)"
    x <- mbind(x, x2)
  }

  if (subtype == "FertilizerProducts") {
    currencyDims <- c("import_kUS$", "export_kUS$")
    xCurrentUSD <- x   # nolint
    x[, , currencyDims] <- convertGDP(x[, , currencyDims],
                                      unit_in = "current US$MER",
                                      unit_out = "constant 2005 US$MER",
                                      replace_NAs = "no_conversion") * 1000
    # for countries with missing conversion factors we assume no inflation:
    x[is.na(x)] <- xCurrentUSD[is.na(x)]

    getNames(x, dim = 2)[getNames(x, dim = 2) == "import_kUS$"] <- "import_US$MER05"
    getNames(x, dim = 2)[getNames(x, dim = 2) == "export_kUS$"] <- "export_US$MER05"

  }

  if (subtype == "Trade") {
    currencyDims <- c("import_kUS$", "export_kUS$")
    xCurrentUSD <- x   # nolint
    x[, , currencyDims] <- convertGDP(x[, , currencyDims],
                                      unit_in = "current US$MER",
                                      unit_out = "constant 2005 US$MER",
                                      replace_NAs = "no_conversion") * 1000
    # for countries with missing conversion factors we assume no inflation:
    x[is.na(x)] <- xCurrentUSD[is.na(x)]

    getNames(x, dim = 2)[getNames(x, dim = 2) == "import_kUS$"] <- "import_US$MER05"
    getNames(x, dim = 2)[getNames(x, dim = 2) == "export_kUS$"] <- "export_US$MER05"

  }
  # ---- Set negative values to 0 (except stock variation) ----

  if (dimExists(3.2, x)) {
    novar <- setdiff(getItems(x, dim = 3.2), "stock_variation")
    x[, , novar][x[, , novar] < 0] <- 0
  }

  return(x)
}
