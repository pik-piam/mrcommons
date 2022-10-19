#' @importFrom magclass ncells setItems
#' @importFrom luscale groupAggregate

convertLUH2v2 <- function(x, subtype) {
  return(toolConv2CountryByCelltype(x, cells = "lpjcell"))
}
