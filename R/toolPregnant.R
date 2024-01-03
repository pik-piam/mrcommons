#' @title toolPregnant
#' @description extra intake kcal/day for pregnant women. The number of pregnant women is
#' computed through the number of 0 year old children each year (Lutz dataset).
#' According to Human energy requirments , Fao (Rome, 2004) , a woman requires an additional
#' food of 845 kcal/day and 675 kcal/day of food on average furing her pregancy and lactation
#' period respectively. According to Naegele?s rule, the mean gestation period is 280 days (40 weeks)
#' and the lactation period 6 month (25 weeks).
#' @param demo demo is the population divided by sex male (M) , female (F) and both (B)
#' and divided by 8 age classes: 0-4, 5-9, 10-14, 15-19, AG1 (20-29), AG2 (30-59), AG3(60-79), AG4(80+)
#' @param reproductive reproductive age classes (on which the energy requiremetns for newborns are distributed)
#' @export


toolPregnant <- function(demo, reproductive) {
  nPreg <- dimSums(demo[, , c("F.0--4", "M.0--4")], dim = "sex") / 5
  pregRerRep <- (nPreg) / dimSums(demo[, , reproductive], dim = c("sex", "age"))
  pregRerRep[is.nan(pregRerRep)] <- 0
  if (any(pregRerRep > 1, na.rm = TRUE)) {
    warning("more than one kid per women in reproductive age. strange")
  }

  intakePregPc <- (40 / 66) * 845 + (26 / 66) * 675
  out <- collapseNames(demo[, , reproductive] * 0 + intakePregPc * pregRerRep)

  return(out)
}
