#' @title toolCombineItems
#' @description Combines several FAO items into one series: sums with na.rm = TRUE so a real
#'   value carries a genuinely-missing sibling, then re-marks all-NA positions as NA (so a
#'   real gap is not turned into a fake 0).
#' @param x magpie object holding the items.
#' @param items Item names to combine.
#' @param dim Dimension to sum over.
#' @return The combined series.
#' @author Bin Lin
#' @seealso
#' [calcAnimalStocks()]
toolCombineItems <- function(x, items, dim) {
  sub   <- x[, , items]
  allNA <- Reduce(`&`, lapply(items, function(it) is.na(sub[, , it])))
  out   <- dimSums(sub, dim = dim, na.rm = TRUE)
  out[allNA] <- NA
  out
}
