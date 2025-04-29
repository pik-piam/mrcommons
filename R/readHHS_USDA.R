#' @title readHHS_USDA
#' @description reads calory requirement for a standardized population from
#' HHS & USDA. 2015. "2015-2020 Dietary Guidelines for Americans."
#' 8. Dietary Guidelines for Americans.
#' https://health.gov/dietaryguidelines/2015/resources/2015-2020_Dietary_Guidelines.pdf.
#' Appendix 2
#'
#' @return Magpie object with results on global level.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [convertLassaletta2014()],
#' [madrat::readSource()]
#'
readHHS_USDA <- function() { # nolint: object_name_linter.
  a <- read.csv("appendix2.csv", sep = ";", header = TRUE)

  a <- as.magpie(a)
  getSets(a) <- c("region", "year", "sex", "age", "activity")
  return(a)
}
