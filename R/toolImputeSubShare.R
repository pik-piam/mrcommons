#' @title toolImputeSubShare
#' @description Imputes a still-missing dairy/laying sub-population as this country's most
#'   recent real share (carried forward) times the year's total, clamped between 0 and the total.
#'   Only genuine NA gaps are eligible; a real reported 0 is never overwritten.
#' @param sub Sub-population series after toolFillStockGaps().
#' @param total Species total the sub-population is part of.
#' @param subRaw Sub-population as read from FAO before gap-filling (distinguishes real 0 from NA).
#' @return Sub-population series with eligible gaps imputed.
#' @author Bin Lin
#' @seealso
#' [calcAnimalStocks()]
toolImputeSubShare <- function(sub, total, subRaw) {
  subArr    <- as.array(collapseNames(sub))[, , 1]
  totalArr  <- as.array(collapseNames(total))[, , 1]
  subRawArr <- as.array(collapseNames(subRaw))[, , 1]

  for (i in seq_len(nrow(subArr))) {
    t   <- totalArr[i, ]
    raw <- subRawArr[i, ]
    # share anchor: only real, raw FAO observations count as evidence, never a value
    # toolFillStockGaps() already estimated (see header)
    validShare <- !is.na(raw) & raw > 0 & !is.na(t) & t > 0
    if (!any(validShare)) next # no real share evidence for this country/category at all
    shareSeries <- ifelse(validShare, raw / t, NA)
    # eligible only where the raw FAO reading was genuinely NA (not a real 0) and
    # toolFillStockGaps() still left it at 0 (no in-reach evidence to resolve it either)
    gap <- is.na(raw) & !is.na(t) & subArr[i, ] == 0 & t > 0
    if (!any(gap)) next

    for (yi in which(gap)) {
      priorValid <- which(validShare[seq_len(yi)])
      if (length(priorValid) == 0) next # no earlier real observation - leave unresolved
      share <- shareSeries[max(priorValid)]
      subArr[i, yi] <- min(max(share * t[yi], 0), t[yi])
    }
  }
  out <- collapseNames(sub)
  out[, , ] <- as.numeric(subArr)
  out
}
