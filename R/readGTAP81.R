#' @title readGTAP81
#' @description Read in data from GTAP 8.1 baseyears 2007 and 2004. All values in mio. current US$MER.
#'
#' @param subtype GTAP header that should be read. Available headers are listed in the contents_xxx.csv in the
#' GTAP81 source folder for each GTAP file (BaseData.har, BaseRate.har, BaseView.har, CO2.har, gsdvole.har,
#' GTAPSam.har, Sets.har, TStrade.har, and Default.prm - file is determined based on the header via a mapping
#' and does not need to be specified).
#' @return GTAP data as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' readSource("GTAP81", subtype = "SF01")
#' }
#' @importFrom reshape2 melt
#' @importFrom magclass as.magpie

readGTAP81 <- function(subtype) {

  # maps headers (variables) in BaseData, BaseRate, BaseView, TStrade, gsdvole, GTAPSam and CO2 to respective files
  fileMapping <- toolGetMapping("GTAP81Header2File.csv", where = "mrcommons")
  file <- fileMapping$file[fileMapping$header == subtype]

  # function to load variable from one GTAP8.1 dataset (either 2004 or 2007)
  .readDataset <- function(file, subtype, year) {
    # load data
    if (requireNamespace("HARr", quietly = TRUE)) {
      GTAP <- HARr::read_har(paste0("FilesGTAP81y", year, "/", file, ".har"))[subtype]
    } else {
      stop(paste("HARr is needed to read data from GTAP. You can install the package via",
                 "devtools::install_git('https://github.com/USDA-ERS/MTED-HARr.git')"))
    }

    GTAP <- melt(GTAP)
    GTAP$REG <- toupper(GTAP$REG)
    if ("REG.1" %in% colnames(GTAP)) {
      GTAP$REG.1 <- toupper(GTAP$REG.1)
      colnames(GTAP)[colnames(GTAP) == "REG.1"] <- "REG2"
    }
    if ("YEAR" %in% colnames(GTAP)) GTAP$YEAR <- tolower(GTAP$YEAR)

    # transform to magclass object
    if ("YEAR" %in% colnames(GTAP)) {
      GTAP <- as.magpie(GTAP, spatial = "REG", temporal = "YEAR")
    } else {
      GTAP <- as.magpie(GTAP, spatial = "REG")
    }
    GTAP <- GTAP[, , , drop = TRUE]
    return(GTAP)
  }

  if (subtype == "VTTS") { # trade time series covering years 1995 - 2009, identical for both GTAP 8.1 baseyears
    GTAP <- .readDataset(file = file, subtype = subtype, year = 2007)
  } else {# all other variables only report one year, 2004 or 2007 depending on baseyear, which we combine here
    GTAP04 <- setYears(.readDataset(file = file, subtype = subtype, year = 2004), "y2004")
    GTAP07 <- setYears(.readDataset(file = file, subtype = subtype, year = 2007), "y2007")
    GTAP <- mbind(GTAP04, GTAP07)
  }

  return(GTAP)
}
