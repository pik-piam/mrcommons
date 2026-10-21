#' @title toolClampToTotal
#' @description Caps a dairy/laying sub-population at its species total. FAO can report more
#'   producing animals than the species total; the excess turns into a negative remainder when
#'   the total is split. A 0 total means missing, not smaller, so those country-years are
#'   counted and left unchanged.
#' @param sub Sub-population series.
#' @param total Species total the sub-population is part of.
#' @param name Category name, used in the reported message.
#' @return The sub-population, capped at the total wherever a total is reported.
#' @author Bin Lin
#' @seealso
#' [calcAnimalStocks()]
toolClampToTotal <- function(sub, total, name) {
  subArr   <- as.array(collapseNames(sub))[, , 1]
  totalArr <- as.array(collapseNames(total))[, , 1]
  over    <- !is.na(subArr) & !is.na(totalArr) & subArr > totalArr & totalArr > 0
  noTotal <- !is.na(subArr) & subArr > 0 & !is.na(totalArr) & totalArr == 0

  if (any(over)) {
    # within 5%: stock is a point count, producing animals a yearly flow - herd turnover
    ratio <- subArr[over] / totalArr[over]
    vcat(2, paste0(sum(over), " ", name, " country-years above the species total; clamped to it (",
                   sum(ratio <= 1.05), " within 5%, ", sum(ratio > 1.05), " beyond, max ",
                   round(max(ratio), 1), "x)"))
    subArr[over] <- totalArr[over]
  }
  if (any(noTotal)) {
    vcat(2, paste0(sum(noTotal), " ", name, " country-years reported without a species total; left unchanged"))
  }

  out <- collapseNames(sub)
  out[, , ] <- as.numeric(subArr)
  out
}
