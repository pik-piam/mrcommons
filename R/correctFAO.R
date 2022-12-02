#' @title correctFAO
#'
#' @description Corrects FAO data for known mismatches or insufficiencies
#'
#' @param x MAgPIE object containing original values
#' @param subtype The FAO file type, e.g.: CBCrop
#'
#' @return Data as MAgPIE object
#' @author Kristine Karstens
#'
#' @seealso [readFAO()], [readSource()],
#' @examples
#' \dontrun{
#' a <- readSource("FAO", "Crop", convert = TRUE)
#' }
#'
correctFAO <- function(x, subtype) {

  if (subtype == "Fodder") x <- x[, , "645|Pumpkins for Fodder", invert = TRUE] # smashing some pumpkins here

  return(x)
}
