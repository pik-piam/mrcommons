#' @title readPyrolysisConditions
#' @description Reads in parameter specifications for different pyrolysis process
#' conditions from several literature sources, which are available as separate
#' source data files.
#'
#' @param subtype Available subtypes: "Schmidt_2019", "Woolf_2014",
#' "Buffi_2024" and "Cornelissen_2016"
#' @return Magpie object with the dimensions "properties" and "process_cond"
#'
#' @author Isabelle Weindl
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("PyrolysisConditions")
#' }

readPyrolysisConditions <- function(subtype = "Schmidt_2019") {

  files <- c(Schmidt_2019 = "Schmidt_2019.csv",
             Woolf_2014 = "Woolf_2014.csv",
             Buffi_2024 = "Buffi_2024.csv",
             Cornelissen_2016 = "Cornelissen_2016.csv")

  file <- toolSubtypeSelect(subtype, files)

  output          <- read.magpie(file)

  if (subtype %in% c("Schmidt_2019")) {
    getNames(output) <- gsub("w.b.", "wm", getNames(output))
    getNames(output) <- gsub("d.b.", "dm", getNames(output))
  }

  getSets(output) <- c("region", "year", "properties", "process_cond")

  return(output)
}
