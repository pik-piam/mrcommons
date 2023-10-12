#' @title calcNitrogenBNF
#' @description calculates fixation from freeliving bacteria and from nitrogen-fixing crops and natural vegetation
#' @param cellular cellular disaggreagation or national values
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcNitrogenFixationPast()]
#' @examples
#' \dontrun{
#' calcOutput("NitrogenBNF")
#' }
#'
calcNitrogenBNF <- function(cellular = FALSE) {
  land <- calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE)
  bnfRate <- calcOutput("NitrogenFixationRateNatural", aggregate = FALSE)
  bnf <- land * bnfRate
  bnf[, , c("crop", "urban")] <- 0

  if (!cellular) {
    bnf <- dimSums(bnf, c("x", "y"))
    bnf <- toolCountryFill(bnf, fill = 0)
  }

  bnf[, , "crop"] <- dimSums(calcOutput("NitrogenFixationPast", aggregate = FALSE, cellular = cellular,
                                        fixation_types = "both", sum_plantparts = TRUE),
                             dim = 3)

  return(list(x = bnf,
              weight = NULL,
              unit = "Mt Nr",
              description = "Natural and anthropogenic nitrogen fixation by vegetation and freeliving bacteria",
              isocountries = !cellular))
}
