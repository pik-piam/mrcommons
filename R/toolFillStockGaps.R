# Which years of a leading/trailing gap may be filled from the nearest real value?
# A year with production reported above 0 is evidence that the herd exists, and so is a
# single-year gap. A year without any production figure is no evidence that the herd is
# gone: it is filled if the series only lacks its last one or two years, or if the
# nearest reported production before and after it is positive. A reported 0 blocks the
# fill. Internal helper of toolFillStockGaps().
toolStockGapSupport <- function(p, s, e, trailing) {
  known   <- which(!is.na(p))
  bridged <- vapply(s:e, function(t) {
    before <- known[known < t]
    after  <- known[known > t]
    length(before) > 0 && length(after) > 0 && p[max(before)] > 0 && p[min(after)] > 0
  }, logical(1))
  recent <- trailing && e - s + 1 <= 2
  (e - s + 1 == 1) | (!is.na(p[s:e]) & p[s:e] > 0) | (is.na(p[s:e]) & (recent | bridged))
}

#' @title toolFillStockGaps
#' @description Fills NA gaps in an FAO census stock series using the matching production
#'   series as evidence: sandwiched gaps are interpolated, leading/trailing gaps filled to
#'   the nearest real value where production is non-zero (or the gap is a single year), the
#'   rest left at 0. A year for which FAO reports no production at all is not evidence that
#'   the herd is gone: it is filled if the series only lacks its last one or two years, or
#'   if the nearest reported production before and after it is positive. Genuine NA gaps
#'   are kept distinct from real reported zeros.
#' @param stock Stock series (magpie object) with genuine gaps marked NA.
#' @param production Matching production series used as fill evidence.
#' @return Stock series with gaps filled.
#' @author Bin Lin
#' @seealso
#' [calcAnimalStocks()]
#' @importFrom stats approx
toolFillStockGaps <- function(stock, production) {
  stockArr <- as.array(collapseNames(stock))[, , 1]
  prodArr  <- as.array(collapseNames(production))[, , 1]
  yrs      <- as.integer(gsub("^y", "", colnames(stockArr)))

  for (i in seq_len(nrow(stockArr))) {
    v <- stockArr[i, ]
    isGap <- is.na(v)
    if (!any(isGap) || all(isGap)) next # no gap, or never had any real data at all
    p <- prodArr[i, ]
    r <- rle(isGap)
    ends   <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1
    for (k in seq_along(r$lengths)) {
      if (!r$values[k]) next
      s <- starts[k]
      e <- ends[k]
      if (s > 1 && e < length(v)) {
        # sandwiched: real values on both sides, no evidence or distance limit needed
        v[s:e] <- approx(x = c(yrs[s - 1], yrs[e + 1]), y = c(v[s - 1], v[e + 1]), xout = yrs[s:e])$y
      } else if (e == length(v) || s == 1) {
        # trailing/leading: single-year gaps fill outright; longer gaps need production
        # evidence in each year
        trailing <- e == length(v)
        anchor   <- if (trailing) v[s - 1] else v[e + 1]
        v[s:e][toolStockGapSupport(p, s, e, trailing)] <- anchor
      }
    }
    stockArr[i, ] <- v
  }
  stockArr[is.na(stockArr)] <- 0 # any gap left unresolved (no evidence) -> 0

  out <- collapseNames(stock)
  out[, , ] <- as.numeric(stockArr)
  out
}
