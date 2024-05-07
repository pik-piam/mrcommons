#' @title readGTAPv8v9
#' @description Read in data from GTAP 8.1 baseyears 2007 and 2004. All values in mio. current US$MER.
#'
#' @param subtype GTAP version and header that should be read. Available versions are "81" and "9"
#'  Available headers are listed in the contents_xxx.csv in the
#' GTAPv8v9 source folder for each GTAP file (BaseData.har, BaseRate.har, BaseView.har, CO2.har, gsdvole.har,
#' GTAPSam.har, Sets.har, TStrade.har, and Default.prm - file is determined based on the header via a mapping
#' and does not need to be specified).
#' @return GTAP data as MAgPIE object
#' @author Debbora Leip, David M. Chen
#' @examples
#' \dontrun{
#' readSource("GTAPv8v9", subtype = "81:SF01")
#' }
#' @importFrom reshape2 melt
#' @importFrom magclass as.magpie
#' @importFrom utils capture.output
readGTAPv8v9 <- function(subtype) {

  split <- toolSplitSubtype(subtype, list(version = NULL, header = NULL))

  # maps headers (variables) in BaseData, BaseRate, BaseView, TStrade, gsdvole, GTAPSam and CO2 to respective files
  if (split$version == "81") {
    fileMapping <- toolGetMapping("GTAP81Header2File.csv", where = "mrcommons")
  } else if (split$version == 9) {
    fileMapping <-    toolGetMapping("GTAP9Header2File.csv", where = "mrcommons")
  }
  file <- fileMapping$file[fileMapping$header == split$header]

  # function to load variable from one GTAP dataset (either 2004 or 2007)
  .readDataset <- function(file, subtype, year) {
    # load data
    if (requireNamespace("HARr", quietly = TRUE)) {
      # suppress progress bar to prevent cluttering log files
      capture.output(
                     {
                       suppressMessages({
                         gtap <- HARr::read_har(paste0("FilesGTAP",
                                 split$version, "y", year, "/", file, ".har"))[tolower(subtype)] #nolint
                       })
                     },
        file = nullfile()) #nolint
    } else {
      stop(paste("HARr is needed to read data from GTAP. You can install the package via",
                 "remotes::install_git('https://github.com/USDA-ERS/MTED-HARr.git')"))
    }

    gtap <- melt(gtap)
    gtap$reg <- toupper(gtap$reg)
    if ("reg.1" %in% colnames(gtap)) {
      gtap$reg.1 <- toupper(gtap$reg.1)
      colnames(gtap)[colnames(gtap) == "reg.1"] <- "reg2"
      spatial <- c("reg", "reg2")
    } else {
      spatial <- "reg"
    }
    if ("year" %in% colnames(gtap)) gtap$year <- tolower(gtap$year)

    # transform to magclass object
    if ("year" %in% colnames(gtap)) {
      gtap <- as.magpie(gtap, spatial = spatial, temporal = "year")
    } else {
      gtap <- as.magpie(gtap, spatial = spatial)
    }
    gtap <- gtap[, , , drop = TRUE]
    return(gtap)
  }

  if (split$header == "VTTS") {
    # bilateral trade time series,
    # identical across GTAP baseyears for a given version
    gtap <- .readDataset(file = file, subtype = split$header, year = 2007)
  } else {
    # all other variables only report one year, 2004 or 2007 depending
    # on baseyear, which we combine here
    gtap04 <- setYears(.readDataset(file = file, subtype = split$header,
                                    year = 2004), "y2004")
    gtap07 <- setYears(.readDataset(file = file, subtype = split$header,
                                    year = 2007), "y2007")
    gtap <- mbind(gtap04, gtap07)

    # v9 has additional baseyear
    if (split$version == "9") {
      gtap11 <- setYears(.readDataset(file = file, subtype = split$header,
                                      year = 2011), "y2011")
      gtap <- mbind(gtap, gtap11)
    }
  }

  return(gtap)
}
