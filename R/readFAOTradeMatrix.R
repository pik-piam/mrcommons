#' Read FAOTradeMatrix
#'
#' Read in FAOSTAT detail trade matrix.
#' FAOSTAT does not balance or harmonize the import/export side reporting.
#' Furthermore, in terms of trade value, exporters are "usuallY" reporting FOB, while importers report CIF.
#' Difference in value, given identical qty,
#' is thus the transport margin and any unharmonized reporting combined.
#' @param subtype subsets of the detailed trade matrix to read in. Very large csv needs to be read in chunks
#' separated by export/import quantities and values, as well as kcr, kli and kothers (not in kcr nor kli)
#' Options are all combinations of c("import_value", "import_qty", "export_value",
#' "export_quantity" X c("kcr", "kli", "kothers"))
#' import is import side reporting while export is export-sde reporting
#' @return FAO data as MAgPIE object
#' @author David C
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("FAOTradeMatrix", "import_value_kcr")
#' }
#' @importFrom data.table fread
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom dplyr summarise filter group_by ungroup %>% distinct
#' @importFrom magpiesets findset

readFAOTradeMatrix <- function(subtype) { # nolint

  file <- "Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"

  # ---- Select columns to be read from file and read file ----

  ## efficient reading of csv file: read only needed columns in the needed type (codes as factor)
  csvcolnames <- colnames(read.table(file, header = TRUE, nrows = 1, sep = ","))

  # check if data is in long or wide format
  long <- ifelse("Year" %in% csvcolnames, TRUE, FALSE)

  # define vector with types corresponding to the columns in the file
  readcolClass <- rep("NULL", length(csvcolnames))
  factorCols <- c("Reporter.Country.Code", "Partner.Country.Code", "Item.Code", "Element.Code", "Element")
  readcolClass[csvcolnames %in% factorCols] <- "factor"
  readcolClass[csvcolnames %in% c("Area", "Country", "Element", "Item", "Unit",
                                  "Months", "Reporter.Countries", "Partner.Countries")] <- "character"
  readcolClass[csvcolnames %in% c("Value", "Year")] <- NA
  if (!long) readcolClass[grepl("Y[0-9]{4}$", csvcolnames)] <- NA

  fao <- suppressWarnings(
                          fread(input = file, header = FALSE, skip = 1, sep = ",",
                            colClasses = readcolClass,
                            col.names = csvcolnames[is.na(readcolClass) | readcolClass != "NULL"],
                            quote = "\"",
                            encoding = "Latin-1", showProgress = FALSE
                          ))
  fao <- as.data.frame(fao)
  # from wide to long (move years from individual columns into one column)
  if (!long) {
    fao <- pivot_longer(fao, cols = starts_with("Y"), names_to = "Year", names_pattern = "Y(.*)",
                        names_transform = list("Year" = as.integer), values_to = "Value")
  }

  names(fao)[names(fao) == "Reporter.Country.Code"] <- "ReporterCountryCode"
  names(fao)[names(fao) == "Partner.Country.Code"] <- "PartnerCountryCode"
  names(fao) <- gsub("\\.", "", names(fao))

  # ---- Assigning the ISO codes to countries ----

  # Load FAO specific countries (not included in country2iso.csv in madrat)
  faoIsoFaoCode <- toolGetMapping("FAOiso_faocode_online.csv", where = "mrcommons")
  # convert data frame into named vector as required by toolCountry2isocode
  faoIsoFaoCode <- structure(as.character(faoIsoFaoCode$ISO), names = as.character(faoIsoFaoCode$Country))

  fao$ReporterISO <- toolCountry2isocode(fao$ReporterCountries, mapping = faoIsoFaoCode)
  fao$PartnerISO <- toolCountry2isocode(fao$PartnerCountries, mapping = faoIsoFaoCode)

  # remove countries with missing ISO code
  fao <- fao[!is.na(fao$ReporterISO), ]
  fao <- fao[!is.na(fao$PartnerISO), ]


  # ---- Reformat elements ----

  elementShort <- toolGetMapping("FAOelementShort.csv", where = "mrcommons")
  # keep relevant rows only
  elementShort <- elementShort[elementShort$ElementCode %in% fao$ElementCode, ]

  # make ElementShort a combination of Element and Unit, replace special characters, and replace multiple _ by one
  tmpElement <- gsub("[\\.,;?\\+& \\/\\-]", "_", fao$Element, perl = TRUE)
  tmpUnit    <- gsub("[\\.,;\\+& \\-]", "_",    fao$Unit, perl = TRUE)
  tmpElementShort <- paste0(tmpElement, "_(", tmpUnit, ")")
  fao$ElementShort <- gsub("_{1,}", "_", tmpElementShort, perl = TRUE) # nolint

  ### replace ElementShort with the entries from ElementShort if the Unit is the same
  if (length(elementShort) > 0) {
    for (i in seq_len(nrow(elementShort))) {
      j <- (fao$ElementCode == elementShort[i, "ElementCode"] & fao$Unit == elementShort[i, "Unit"])
      fao$ElementShort[j] <- as.character(elementShort[i, "ElementShort"])
    }
  }

  # remove accent in Mate to avoid problems and remove other strange names
  fao$Item <- gsub("\u00E9", "e", fao$Item, perl = TRUE)
  fao$Item <- gsub("\n + (Total)", " + (Total)", fao$Item, fixed = TRUE)
  fao$ItemCodeItem <- paste0(fao$ItemCode, "|", gsub("\\.", "", fao$Item, perl = TRUE))

  # some small islands correspond to the same ISO3code, just remove them for now
  fao <- filter(fao, !.data$ReporterCountries %in% c("Johnston Island", "Midway Island",
                                                     "Canton and Enderbury Islands", "Wake Island"),
                !.data$PartnerCountries %in% c("Johnston Island", "Midway Island",
                                               "Canton and Enderbury Islands", "Wake Island"))

  fao <- unite(fao, col = "ISO", c(.data$ReporterISO, .data$PartnerISO), sep = ".", remove = FALSE)

  # subset by both trade column and product column
  mapping <- toolGetMapping("newFAOitems_online_DRAFT.csv", type = "sectoral", where = "mrcommons")
  mapping <- mapping[, c("new_FAOoriginalItem_fromWebsite", "k")]
  colnames(mapping)[1] <- "ItemCodeItem"
  mapping <- distinct(mapping)


  fao <- inner_join(fao, mapping)

  kcr <- findset("kcr")
  kli <- findset("kli")
  kothers <- setdiff(findset("kall"), c(kcr, kli))

  elements <- list(
    import_value_kcr = list(trade = "import_kUS$", product = kcr),
    import_value_kli = list(trade = "import_kUS$", product = kli),
    import_value_kothers = list(trade = "import_kUS$", product = kothers),
    import_qty_kcr = list(trade = c("import", "Import_Quantity_(1000_Head)",
                                    "Import_Quantity_(Head)", "Import_Quantity_(no)"),
                          product = kcr),
    import_qty_kli = list(trade = c("import", "Import_Quantity_(1000_Head)",
                                    "Import_Quantity_(Head)", "Import_Quantity_(no)"),
                          product = kli),
    import_qty_kothers = list(trade = c("import", "Import_Quantity_(1000_Head)",
                                        "Import_Quantity_(Head)", "Import_Quantity_(no)"),
                              product = kothers),
    export_value_kcr = list(trade = "export_kUS$", product = kcr),
    export_value_kli = list(trade = "export_kUS$", product = kli),
    export_value_kothers = list(trade = "export_kUS$", product = kothers),
    export_qty_kcr = list(trade = c("export", "Export_Quantity_(1000_Head)",
                                    "Export_Quantity_(Head)", "Export_Quantity_(no)"),
                          product = kcr),
    export_qty_kli = list(trade = c("export", "Export_Quantity_(1000_Head)",
                                    "Export_Quantity_(Head)", "Export_Quantity_(no)"),
                          product = kli),
    export_qty_kothers = list(trade = c("export", "Export_Quantity_(1000_Head)",
                                        "Export_Quantity_(Head)", "Export_Quantity_(no)"),
                              product = kothers)
  )

  element <- toolSubtypeSelect(subtype, elements)

  out <- filter(fao, .data$ElementShort %in% element$trade, .data$k %in% element$product)

  out <- as.magpie(out[, c("Year", "ISO", "ItemCodeItem", "ElementShort", "Value")],
                   temporal = 1, spatial = 2, datacol = 5)   # import/export unit is in tonnes
  getItems(out, dim = 1, raw = TRUE) <- gsub("_", ".", getItems(out, dim = 1))
  gc()


  out <- magpiesort(out)

  return(out)
}
