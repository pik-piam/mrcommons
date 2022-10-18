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
#' @importFrom utils capture.output
readGTAP81 <- function(subtype) {

  # maps headers (variables) in BaseData, BaseRate, BaseView, TStrade, gsdvole, GTAPSam and CO2 to respective files
  fileMapping <- toolGetMapping("GTAP81Header2File.csv", where = "mrcommons")
  file <- fileMapping$file[fileMapping$header == subtype]

  # function to load variable from one GTAP8.1 dataset (either 2004 or 2007)
  .readDataset <- function(file, subtype, year) {
    # load data
    if (requireNamespace("HARr", quietly = TRUE)) {
      # suppress progress bar to prevent cluttering log files
      capture.output({
        suppressMessages({
          gtap <- HARr::read_har(paste0("FilesGTAP81y", year, "/", file, ".har"))[subtype]
        })
      }, file = nullfile())
    } else {
      stop(paste("HARr is needed to read data from GTAP. You can install the package via",
                 "remotes::install_git('https://github.com/USDA-ERS/MTED-HARr.git')"))
    }

    gtap <- melt(gtap)
    gtap$REG <- toupper(gtap$REG)
    if ("REG.1" %in% colnames(gtap)) {
      gtap$REG.1 <- toupper(gtap$REG.1)
      colnames(gtap)[colnames(gtap) == "REG.1"] <- "REG2"
    }
    if ("YEAR" %in% colnames(gtap)) gtap$YEAR <- tolower(gtap$YEAR)

    # transform to magclass object
    if ("YEAR" %in% colnames(gtap)) {
      gtap <- as.magpie(gtap, spatial = "REG", temporal = "YEAR")
    } else {
      gtap <- as.magpie(gtap, spatial = "REG")
    }
    gtap <- gtap[, , , drop = TRUE]
    return(gtap)
  }

  if (subtype == "VTTS") { # trade time series covering years 1995 - 2009, identical for both GTAP 8.1 baseyears
    gtap <- .readDataset(file = file, subtype = subtype, year = 2007)
  } else {# all other variables only report one year, 2004 or 2007 depending on baseyear, which we combine here
    gtap04 <- setYears(.readDataset(file = file, subtype = subtype, year = 2004), "y2004")
    gtap07 <- setYears(.readDataset(file = file, subtype = subtype, year = 2007), "y2007")
    gtap <- mbind(gtap04, gtap07)
  }

  return(gtap)
}
