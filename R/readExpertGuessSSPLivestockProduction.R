#' @title readExpertGuessSSPLivestockProduction
#' @description {Read the Expert Guesses for future Livestock Production for the SSP Scenarios
#' }
#'
#'
#'
#'
#' @param subtype : Available subtypes are: ssp1 to 5 Data for the SSP Scenario
#'
#'
#' @return magpie object containing the expert guesses
#' @author Stephen Wirth, Isabelle Weindl
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("ExpertGuessSSPLivestockProduction","ssp1")
#' }


readExpertGuessSSPLivestockProduction <- function(subtype) {

  # input data version
  ver <- "2025-06"

  files <- c(
    ssp1 = "animal_productivity_expert_guess_ssp1.csv",
    ssp2 = "animal_productivity_expert_guess_ssp2.csv",
    ssp3 = "animal_productivity_expert_guess_ssp3.csv",
    ssp4 = "animal_productivity_expert_guess_ssp4.csv",
    ssp5 = "animal_productivity_expert_guess_ssp5.csv"
  )

  file <- toolSubtypeSelect(subtype, files)

  x <- read.magpie(file.path(ver, file))
  return(x)
}
