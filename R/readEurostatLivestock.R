
#' Read EUROSTAT_livestock
#'
#' Read in EUROSTAT livestock data
#'
#' @param subtype Type of FAO data that should be read. Available types are:
#' \itemize{
#' \item    `EggPopProd` : Production of eggs for consumption and number of laying hens
#' \item    `LayingHensPop` : Laying hens population - annual data
#' \item    `PoultryPop` : Poultry - annual data
#' \item    `PigPop` : Pig population - annual data
#' \item    `BovinePop` : Bovine population - annual data
#' \item    `Nuts2Pop` : Animal populations by NUTS 2 regions
#' \item    `MeatProd` : Meat production and foreign trade - head - monthly data
#' \item    `MilkNuts2Prod` : Production of cow's milk on farms by NUTS 2 regions
#' \item    `MilkProd` : Production and utilization of milk on the farm - annual data
#' }
#' @return EUROSTAT livestock data as MAgPIE object
#' @author David Hötten
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("EurostatLivestock", "MeatProd")
#' }
#' @importFrom utils read.table head
#' @importFrom dplyr mutate mutate_all filter `%>%` across rename
#' @importFrom tidyr pivot_longer starts_with matches
#' @importFrom rlang .data
#' @importFrom countrycode countrycode


readEurostatLivestock <- function(subtype) {
  # ----- read in the raw data file -----
  # define files corresponding to subtypes
  files <- c(
    EggPopProd = "apro_ec_egghen",
    LayingHensPop = "apro_ec_lshen",
    PoultryPop = "apro_ec_poula",
    PigPop = "apro_mt_lspig",
    BovinePop = "apro_mt_lscatl",
    Nuts2Pop = "agr_r_animal",
    MeatProd = "apro_mt_pheadm",
    MilkNuts2Prod = "agr_r_milkpr",
    MilkProd = "apro_mk_farm"
  )

  file <- toolSubtypeSelect(subtype, files)

  file <- paste0(file, ".tsv")

  table <-
    read.table(file,
               sep = "\t",
               header = TRUE,
               stringsAsFactors = FALSE)

  df <- as.data.frame(table)

  # ----- some general data cleaning -----
  # split the first column at all "."
  colToSeperate <- names(df)[1]
  colSplitted <- head(unlist(strsplit(colToSeperate, "[.]")), -1)

  df <- tidyr::separate(df, colToSeperate, colSplitted, sep = ",")

  # remove all characters from all columns starting with "X" i.e. data columns
  # these characters are used to give special meatadata to individual cells
  # in Eurostat data, e.g. p <-> provisional
  df <-
    df %>% dplyr::mutate(across(starts_with("X") &
                                  matches("[1-9]"), ~ gsub("[a-z]", "", .)))


  # convert all ": " to NA
  df <- df %>% dplyr::mutate_all(~ ifelse(. == ": ", NA, .))

  # pivot to long format
  df <-
    tidyr::pivot_longer(
      df,
      cols = -c(1:length(colSplitted)), # nolint: seq_liner
      names_to = "year",
      values_to = "value"
    )
  df <- df %>% dplyr::mutate(value = as.numeric(.data$value))

  # replace "X" by "y" in the year column
  df$year <- gsub("X", "y", df$`year`)


  # ---- get the geo column in the correct format ----
  # filter out row with geo starting with "EU", which contain EU aggregates
  df <- df %>% dplyr::filter(!grepl("EU", .data$geo))

  # sepearate the "geo" column into country and nuts extension,
  # first 2 letters are country, rest is extension
  if (any(nchar(df$geo) > 2)) {
    df <- tidyr::separate(df, .data$geo, c("country", "nutsExt"), sep = 2)
  } else {
    df <- dplyr::rename(df, country = .data$geo)
  }

  # change alpha 2 to alpha 3 country codes with the countrycode package
  df$country <-
    countrycode::countrycode(df$country,
                             "iso2c",
                             "iso3c",
                             custom_match = c("UK" = "GBR", "EL" = "GRC", "XK" = "KOS"))

  # if nutsExt exists, combine country and nutsExt to a new column country seperated by a "."
  if ("nutsExt" %in% names(df)) {
    df$country <- paste0(df$country, ".", df$nutsExt)
    # rename country column to "country.nuts"
    names(df)[names(df) == "country"] <- "country.nuts"
    # remove nutsExt column
    df <- df %>% dplyr::select(-.data$nutsExt)
    nuts <- TRUE
  } else {
    nuts <- FALSE
  }

  # ---- create MAgPIE object ----

  # create a multidimensional array
  array <- xtabs(value ~ ., data = df, na.action = "na.pass")
  class(array) <- "array"

  # get names of the array
  names <- names(dimnames(array))

  # convert to MAgPIE object
  out <-
    as.magpie(
      array,
      spatial =  which(names == ifelse(nuts, "country.nuts", "country")),
      temporal = which(names == "year")
    )

  # if an item of the year dim contains a ., change the name of
  # the year column to "year.month", to separate the year and month dimension
  dm <- getItems(out)[2]
  if (any(grepl("\\.", dm))) {
    getSets(out)[2] <- "year.month"
  }


  return(out)

}
