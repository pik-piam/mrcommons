#' @importFrom magclass ncells setItems
#' @importFrom luscale groupAggregate

convertLUH2v2 <- function(x, subtype) {
  x   <- toolCoord2Isocell(x, cells = "lpjcell")
  map <- data.frame(from = getItems(x, dim = 1),
                    to = getItems(x, dim = 1.1, full = TRUE))
  out <- toolAggregate(x, map)
  getSets(out, fulldim = FALSE)[1] <- "country"
  out  <- toolCountryFill(out, fill = 0, verbosity = 2)
  return(out)
}
