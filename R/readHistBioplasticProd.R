#' @title readHistBioplasticProd
#' @description reads data on historic bioplastic production from csv
#' (source: "Biopolymers facts and statistics 2021" page 58, IfBB)
#' @return data as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#'   readSource("HistBioplasticProd")
#' }

readHistBioplasticProd <- function() {

  x <- read.csv("HistBioplasticProd.csv", skip = 5)
  x <- as.magpie(x)

  return(x)
}
