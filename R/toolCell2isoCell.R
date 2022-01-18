#' toolCell2isoCell
#'
#' Sets cell names to "iso country code"."cell number"
#' @param x magpie object on cellular level
#' @param cells switch between magpie cells (59199) and lpj cells (67420)
#' @return return changed input data
#' @author Kristine Karstens
#'
#' @importFrom utils read.csv
#' @export

toolCell2isoCell <- function(x, cells = "magpiecell") {

  if (cells == "magpiecell") {
    cellToCellIso <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")
    getCells(x)   <- cellToCellIso$celliso
  }

   return(x)
}
