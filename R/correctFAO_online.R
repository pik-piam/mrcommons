#' @title correctFAO_online
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
#' a <- readSource("FAO_online", "Crop", convert = TRUE)
#' }
#'
correctFAO_online <- function(x, subtype) { # nolint: object_name_linter.

  if (subtype == "Fodder") x <- x[, , "645|Pumpkins for Fodder", invert = TRUE] # smashing some pumpkins here

  return(x)
}
