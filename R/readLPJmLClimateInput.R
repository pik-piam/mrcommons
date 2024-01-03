#' @title readLPJmLClimateInput
#' @description Read Climate data used as LPJmL inputs into MAgPIE objects
#' @param subtype Switch between different inputs,
#'                e.g. "ISIMIP3bv2:MRI-ESM2-0:ssp370:1850-2014:tas"
#'                Available variables are: * tas -
#'                                         * wet -
#'                                         * per -
#' @param subset Switch between different subsets of the same subtype
#'               Available options are: "annualMean", "annualSum",
#'                                      "monthlyMean", "monthlySum",
#'                                      "wetDaysMonth"
#'               Note that not all subtype-subset combinations make sense
#' @return MAgPIE objects with results on cellular level.
#' @author Marcos Alves, Kristine Karstens, Felicitas Beier
#' @seealso
#' \code{\link{readLPJmLClimateInput}}
#' @examples
#' \dontrun{
#' readSource("LPJmLClimateInput", subtype, convert = "onlycorrect")
#' }
#'
#' @importFrom lpjclass read.LPJ_input
#' @importFrom madrat toolSplitSubtype
#' @importFrom magpiesets findset addLocation
#' @importFrom magclass collapseNames collapseDim as.magpie clean_magpie
#' @export

readLPJmLClimateInput <- function(subtype = "ISIMIP3bv2:MRI-ESM2-0:ssp370:temperature", # nolint
                                  subset  = "annualMean") {

  nCells  <- 67420 # number of cells in lpjml
  subtype <- toolSplitSubtype(subtype,
                              list(version      = NULL,
                                   climatemodel = NULL,
                                   scenario     = NULL,
                                   variable     = NULL))

  subsetTypes     <- c("annualMean", "annualSum", "monthlyMean",
                       "monthlySum", "wetDaysMonth", "\\d{4}:\\d{4}")
  subsetTypesMean <- c(grep("Mean", subsetTypes, value = TRUE), "\\d{4}:\\d{4}")

  allowedCombos <- list(temperature    = subsetTypesMean,
                        precipitation  = subsetTypes,
                        longWaveNet    = subsetTypesMean,
                        shortWave      = subsetTypesMean,
                        temperatureMin = subsetTypesMean,
                        temperatureMax = subsetTypesMean)
  isAllowed     <- any(vapply(allowedCombos[[subtype$variable]],
                              grepl, x = subset,
                              FUN.VALUE = logical(1)))
  if (!isAllowed) stop("Subtype-subset combination not allowed")

  .prepareLPJinput <- function(subset = NULL) {

    filename <- Sys.glob(c("*.bin", "*.clm"))
    filetype <- tail(unlist(strsplit(filename, "\\.")), 1)

    if (filetype == "clm") {

      filedata     <- file(description = filename, open = "rb",
                           blocking = TRUE, encoding = getOption("encoding"))
      seek(filedata, where = 15, origin = "start")
      inHeader     <- as.numeric(readBin(filedata, what = integer(), size = 4,
                                         n = 5, endian = .Platform$endian))
      startyear    <- inHeader[1]
      nyear        <- inHeader[2]
      noAnnualPredictions <- inHeader[5]
      years        <- seq(startyear, startyear + nyear - 1, 1)
      close(filedata)

    } else {
      stop("File format of LPJmLClimateInput data unknown. Please provide .clm file format.")
    }

    if (subset == "wetDaysMonth") {

      if (subtype$variable != "precipitation") stop("Subset 'wetDaysMonth' is only
                                                   available for 'precipitation'")
      x <- lpjclass::read.LPJ_input(file_name   = filename,
                                    out_years   = paste0("y", years),
                                    namesum     = TRUE,
                                    ncells      = nCells,
                                    rule4binary = ">0") / noAnnualPredictions

      class(x) <- "array"
      x        <- collapseNames(as.magpie(x, spatial = 1))


    } else if (subset == "annualMean") {

      x <- lpjclass::read.LPJ_input(file_name   = filename,
                                    out_years   = paste0("y", years),
                                    namesum     = TRUE,
                                    ncells      = nCells) / noAnnualPredictions

      class(x) <- "array"
      x        <- collapseNames(as.magpie(x, spatial = 1))


    } else if (subset == "annualSum") {

      x <- lpjclass::read.LPJ_input(file_name   = filename,
                                    out_years   = paste0("y", years),
                                    namesum     = TRUE,
                                    ncells      = nCells)

      class(x) <- "array"
      x        <- collapseNames(as.magpie(x, spatial = 1))


    } else if (subset %in% c("monthlyMean", "monthlySum")) {
      # define year sets (cut it in bunches)
      bunchLength <- 1
      yearsets    <- split(years, ceiling(seq_along(years) / bunchLength))

      # define month mapping
      monthLength <- c(jan = 31, feb = 28, mar = 31, apr = 30,
                       may = 31, jun = 30, jul = 31, aug = 31,
                       sep = 30, oct = 31, nov = 30, dec = 31)
      daysMonth   <- NULL
      for (m in 1:12) {
        daysMonth <- c(daysMonth, rep(names(monthLength[m]),
                                      monthLength[m]))
      }
      month2day   <- cbind(day   = 1:sum(monthLength),
                           month = daysMonth)
      monthLength <- as.magpie(monthLength)

      # create output object
      x <- NULL

      # loop over bunches
      for (b in seq_along(yearsets)) {
        # read in a bunch of years
        tmp <- lpjclass::read.LPJ_input(file_name = filename,
                                        out_years = paste0("y", yearsets[[b]]),
                                        namesum   = FALSE,
                                        ncells    = nCells)

        tmp <- array(tmp, dim = dim(tmp)[1:3], dimnames = dimnames(tmp)[1:3])
        tmp <- as.magpie(tmp, spatial = 1)
        getSets(tmp) <- c("fake", "year", "day")
        # KRISTINE: Please double-check whether following line makes sense
        # (introduced because toolAggregate doesn't work without dimension names)
        getNames(tmp) <- as.character(seq(1, 365, 1))

        # aggregate days to month
        tmp <- toolAggregate(tmp,
                             rel  = month2day,
                             from = "day",
                             to   = "month",
                             dim  = 3)

        if (subset == "monthlyMean") {
          tmp <- tmp / monthLength
        }

        x <- mbind(x, tmp)
        getSets(x) <- c("fake", "year", "month")
      }

    } else if (grepl("\\d{4}:\\d{4}", subset)) {

      subYears <- eval(parse(text = subset))
      years    <- intersect(years, subYears)
      if (any(!(subYears %in% years))) {
        warning(paste0("Some subsetted years (subset = ", subset,
                ") are not availabl\n in the original data.\n",
                "Years set to:", years))
      }

      x <- lpjclass::read.LPJ_input(file_name   = filename,
                                    out_years   = paste0("y", years),
                                    namesum     = FALSE,
                                    ncells      = nCells)

      class(x) <- "array"
      x        <- collapseNames(as.magpie(x, spatial = 1))


    } else {
      stop("Subset argument unknown. Please check function help.")
    }

    return(x)
  }

  x <- .prepareLPJinput(subset)

  # Add location based on LPJmL cell ordering where fist cell is FJI, second RUS, etc
  x <- collapseDim(addLocation(x), dim = c("N", "region"))
  x <- clean_magpie(x)

  return(x)
}
