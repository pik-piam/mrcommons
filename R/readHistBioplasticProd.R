#' @title readIfBB
#' @description reads data on historic bioplastic production from csv
#' source: "Biopolymers facts and statistics 2021" page 58, Institute for Bioplastics and Biocomposites (IfBB)
#' @return data as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   readSource("HistBioplasticProd")
#' }

readIfBB <- function() {

  x <- read.csv("HistBioplasticProd.csv", skip = 5)
  x <- as.magpie(x)

  return(x)
}
