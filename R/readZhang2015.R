#' Read Nitrogen Use Efficiency Paramerst from Zhang et al 2015
#'
#' Publication: Zhang, Xin, Eric A. Davidson, Denise L. Mauzerall, Timothy D. Searchinger, Patrice Dumas, and Ye Shen.
#' 2015. "Managing Nitrogen for Sustainable Development". Nature 528 (7580): 51–59. https://doi.org/10.1038/nature15743.

#' @return MAgPIE object
#' @author Benjamin Leon Bodirsky
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("Zhang2015", convert = FALSE)
#' }
#'
readZhang2015 <- function() {
  nue <- read.magpie("targetNUE.csv")
  getSets(nue) <- c("country", "year", "variable")
  return(nue)
}
