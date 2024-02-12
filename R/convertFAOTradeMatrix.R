#' Convert FAOTradeMatrix
#'
#' Convert FAOSTAT detailed trade matrix.
#' FAOSTAT does not balance or harmonize the import/export side reporting.
#' Furthermore, in terms of trade value, exporters are "usuallY" reporting FOB,
#' while importers report CIF. Difference in value,
#' given identical qty, is thus the transport margin mixed with unharmonized reporting.
#' @param x output from read function
#' @param subtype subsets of the detailed trade matrix to read in. Very large csv needs to be read in chunks
#' separated by export/import quantities and values, as well as kcr, kli and kothers (not in kcr nor kli)
#' Options are all combinations of c("import_value",
#' "import_qty", "export_value", "export_quantity") X c("kcr", "kli", "kothers"))
#' import is import side reporting while export is export-sde reporting
#' @return FAO data as MAgPIE object
#' @author David C
#' @seealso [readSource()]
#' @importFrom GDPuc convertGDP


convertFAOTradeMatrix <- function(x, subtype) { # nolint

  gc()

  # ---- Section for country specific treatment ----

  # make a set name for dim 1.2
  getSets(x)[1] <- "ISO.Partner"

  ## data for Eritrea ERI and South Sudan SSD added with 0 if not existing after the split
  ## to make toolISOhistorical work
  if (any(getItems(x, dim = 1.1) == "XET") && any(getItems(x, dim = 1.1) == "ETH") &&
        !any(getItems(x, dim = 1.1) == "ERI")) {
    xERI <- x[list("ISO" = c("ETH")), , ]
    xERI[, , ] <- 0
    getItems(xERI, dim = 1.1) <- "ERI"
    missingC <- paste0("ERI.",
                       setdiff(getItems(x, dim = 1.2), getItems(xERI, dim = 1.2)))
    fillC <- new.magpie(cells_and_regions = missingC, years = getYears(x), names = getNames(x), fill = 0)
    xERI <- mbind(xERI, fillC)
    x <- magpiesort(mbind(x, xERI))
  }
  if (any(getItems(x, dim = 1.2) == "XET") && any(getItems(x, dim = 1.2) == "ETH") &&
        !any(getItems(x, dim = 1.2) == "ERI")) {
    xERI <- x[list("Partner" = c("ETH")), , ]
    xERI[, , ] <- 0
    getItems(xERI, dim = 1.2) <- "ERI"
    missingC <- paste0(".ERI",
                       setdiff(getItems(x, dim = 1.1), getItems(xERI, dim = 1.1)))
    fillC <- new.magpie(cells_and_regions = missingC, years = getYears(x), names = getNames(x), fill = 0)
    xERI <- mbind(xERI, fillC)
    x <- magpiesort(mbind(x, xERI))
  }

  if (any(getItems(x, dim = 1.1) == "XSD") && any(getItems(x, dim = 1.1) == "SDN") &&
        !any(getItems(x, dim = 1.1) == "SSD")) {
    xSSD <- x[list("ISO" = c("SDN")), , ]
    xSSD[, , ] <- 0
    getItems(xSSD, dim = 1.1) <- "SSD"
    missingC <- paste0("SSD.",
                       setdiff(getItems(x, dim = 1.2), getItems(xSSD, dim = 1.2)))
    xSSD <- mbind(xSSD, fillC)
    x <- magpiesort(mbind(x, xSSD))
  }

  if (any(getItems(x, dim = 1.2) == "XSD") && any(getItems(x, dim = 1.2) == "SDN") &&
        !any(getItems(x, dim = 1.2) == "SSD")) {
    xSSD <- x[list("Partner" = c("SDN")), , ]
    xSSD[, , ] <- 0
    getItems(xSSD, dim = 1.2) <- "SSD"
    missingC <- paste0(".SSD",
                       setdiff(getItems(x, dim = 1.1), getItems(xSSD, dim = 1.1)))
    xSSD <- mbind(xSSD, fillC)
    x <- magpiesort(mbind(x, xSSD))
  }

  ## add additional mappings
  additionalMapping <- list()

  # Eritrea ERI and Ethiopia ETH
  if ((all(c("XET", "ETH", "ERI") %in% getItems(x, dim = 1.1))) ||
        (all(c("XET", "ETH", "ERI") %in% getItems(x, dim = 1.2)))) {
    additionalMapping <- append(additionalMapping,
                                list(c("XET", "ETH", "y1992"), c("XET", "ERI", "y1992")))
  }

  # Belgium-Luxemburg
  if ((all(c("XBL", "BEL", "LUX") %in% getItems(x, dim = 1.1))) ||
        (all(c("XBL", "BEL", "LUX") %in% getItems(x, dim = 1.2)))) {
    additionalMapping <- append(additionalMapping,
                                list(c("XBL", "BEL", "y1999"), c("XBL", "LUX", "y1999")))
  } else if (("XBL" %in% getItems(x, dim = 1.1)) && !("BEL" %in% getItems(x, dim = 1.1))) {
    getCells(x)[getItems(x, dim = 1.1) == "XBL"] <- "BEL"
  } else if (("XBL" %in% getItems(x, dim = 1.2)) && !("BEL" %in% getItems(x, dim = 1.2))) {
    getCells(x)[getItems(x, dim = 1.2) == "XBL"] <- "BEL"
  }

  # Sudan (former) to Sudan and Southern Sudan. If non of the latter two is in the data make Sudan (former) to Sudan
  if ((all(c("XSD", "SSD", "SDN") %in% getItems(x, dim = 1.1))) ||
        (all(c("XSD", "SSD", "SDN") %in% getItems(x, dim = 1.2)))) {
    additionalMapping <- append(additionalMapping,
                                list(c("XSD", "SSD", "y2011"), c("XSD", "SDN", "y2011")))
  } else if ("XSD" %in% getItems(x, dim = 1.1) && !any(c("SSD", "SDN") %in% getItems(x, dim = 1.1))) {
    getCells(x)[getItems(x, dim = 1.1) == "XSD"] <- "SDN"
  } else if ("XSD" %in% getItems(x, dim = 1.2) && !any(c("SSD", "SDN") %in% getItems(x, dim = 1.2))) {
    getCells(x)[getItems(x, dim = 1.2) == "XSD"] <- "SDN"
  }

  ## if XCN exists, replace CHN with XCN.
  if ("XCN" %in% getItems(x, dim = 1.1)) {
    if ("CHN" %in% getItems(x, dim = 1.1)) x <- x[list("ISO" = c("CHN")), , , invert = TRUE]
    getItems(x, dim = 1.1)[getItems(x, dim = 1.1) == "XCN"] <- "CHN"
  }
  if ("XCN" %in% getItems(x, dim = 1.2)) {
    if ("CHN" %in% getItems(x, dim = 1.2)) x <- x[list("Partner" = c("CHN")), , , invert = TRUE]
    getItems(x, dim = 1.2)[getItems(x, dim = 1.2) == "XCN"] <- "CHN"
  }
  ## data for the Netherlands Antilles is currently removed because currently no
  ## information for its successors SXM, CUW, ABW is available as input for toolISOhistorical
  if (any(getItems(x, dim = 1.1) == "ANT")) {
    x <- x[list("ISO" = c("ANT")), , , invert = TRUE]
  }
  if (any(getItems(x, dim = 1.2) == "ANT")) {
    x <- x[list("Partner" = c("ANT")), , , invert = TRUE]
  }
  ## data for PCI split up into:
  # Marshall Islands (MH, MHL, 584)
  # Micronesia, Federated States of (FM, FSM, 583)
  # Northern Mariana Islands (MP, MNP, 580)
  # Palau (PW, PLW, 585)
  if (all(c("PCI", "MHL", "FSM", "MNP", "PLW") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping,
                                list(c("PCI", "MHL", "y1991"), c("PCI", "FSM", "y1991"),
                                     c("PCI", "MNP", "y1991"), c("PCI", "PLW", "y1991")))
  } else if ("PCI" %in% getItems(x, dim = 1.1)) {
    x <- x[list("ISO" = c("PCI")), , invert = TRUE]
  }
  if (all(c("PCI", "MHL", "FSM", "MNP", "PLW") %in% getItems(x, dim = 1.2))) {
    additionalMapping <- append(additionalMapping,
                                list(c("PCI", "MHL", "y1991"), c("PCI", "FSM", "y1991"),
                                     c("PCI", "MNP", "y1991"), c("PCI", "PLW", "y1991")))
  } else if ("PCI" %in% getItems(x, dim = 1.2)) {
    x <- x[list("Partner" = c("PCI")), , invert = TRUE]
  }

  # some of the follow up states of the Soviet Union (SUN), Yugoslavia (YUG),
  #          Serbia and Montenegro (SCG) are missing add them with values of 0
  isoHistorical <- read.csv2(system.file("extdata", "ISOhistorical.csv",
                                         package = "madrat"), stringsAsFactors = FALSE)
  former <- isoHistorical[isoHistorical$fromISO %in% c("SUN", "YUG", "SCG"), "toISO"]

  missing1 <- former[!former %in% getItems(x, dim = 1.1)]
  if (length(missing1 > 0)) {
    x2 <- new.magpie(cells_and_regions = missing1, years = getYears(x), names = getNames(x))
    x2 <- add_dimension(x2, dim = 1.2, add = "Partner", nm = getItems(x, dim = 1.2))
    x2[, getYears(x2)[getYears(x2, as.integer = TRUE) >= 1992], ] <- 0
    x <- mbind(x, x2)
  }
  missing2 <- former[!former %in% getItems(x, dim = 1.2)]
  if (length(missing2 > 0)) {
    x2 <- new.magpie(cells_and_regions = missing2, years = getYears(x), names = getNames(x))
    x2 <- add_dimension(x2, dim = 1.2, add = "ISO", nm = getItems(x, dim = 1.1))
    x2[, getYears(x2)[getYears(x2, as.integer = TRUE) >= 1992], ] <- 0
    x <- mbind(x, x2)
  }

  x[is.na(x)] <- 0

  ### do ISOhistorical
  x <- toolISOhistorical(x, mapping = NULL, overwrite = TRUE, additional_mapping = additionalMapping)

  out <- toolCountryFillBilateral(x, fill = 0)
  rm(x)
  gc()
  # currency convert values
#  if (subtype %in% c("import_value_kcr", "import_value_kli", "import_value_kothers", #nolint
#                       "export_value_kcr", "export_value_kli", "export_value_kothers")) { #nolint
#       out <- convertGDP(out, unit_in = "current US$MER", #nolint
#                        unit_out = "constant 2005 US$MER", #nolint
#                       replace_NAs = "no_conversion") #nolint
#                      } #nolint

  out <- magpiesort(out)

  return(out)
}
