#' Read allometric coeffizients for residue to harvest translation
#'
#' Read-in a file containing the allometric coeffizients of MAgPIE crop types.
#' Values are assembled from various literature sources, and the weighting and allocation is done in the
#' spreadsheet crop_specifications_*.ods for different versions of the file
#'
#' @return magpie object with the dimension crops and coeffizients
#' @author Benjamin Leon Bodrisky, Kristine Karstens
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("HI")
#' }
#' @importFrom magclass as.magpie getYears

readHI <- function() {

  folder <- ""
  if (!is.null(tmp <- getOption("hi_folder"))) folder <- tmp

  hi <- read.csv(paste0(folder, "hi.csv")) # , header=TRUE)
  hi <- as.magpie(hi, datacol = 2)
  getYears(hi) <- "y2000"
  return(hi)
}
