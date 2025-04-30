#' @title calcAgProductionValue
#'
#' @description  Calculate FAO Value Of Production
#'
#' @param datasource Currently available: `"FAO"`
#'
#' @return FAO Value Of Production as a list of MAgPIE objects
#'
#' @author Roman Popov, Mishko Stevanovic, Patrick v. Jeetze
#' @seealso [madrat::calcOutput()], [mrfaocore::readFAO()],
#' [mrfaocore::convertFAO()], [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- calcOutput("AgProductionValue", datasource = "FAO")
#' }
#'
calcAgProductionValue <- function(datasource = "FAO") {

  if (datasource == "FAO") {
    data <- readSource("FAO_online", "ValueOfProd")
    data <- data[, , "Gross_Production_Value_(USDMER05)_(1000_US$)"] / 1000
    data <- collapseNames(data)

    aggregation <- toolGetMapping("FAOitems.csv", type = "sectoral", where = "mappingfolder")

    data[is.na(data)] <- 0

    # remove data that contains the aggregate categories
    data <- data[, , -grep("Total", getNames(data), fixed = TRUE)]
    # remove live weight data
    if (length(grep("PIN", getNames(data), fixed = TRUE)) > 0) {
      data <- data[, , -grep("PIN", getNames(data), fixed = TRUE)]
    }

    out <- toolAggregate(data, rel = aggregation, from = "ProductionItem", to = "k",
                         dim = 3.1, partrel = TRUE, verbosity = 2)

    out <- collapseNames(out)
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
    out <- add_dimension(out, dim = 3.2, add = "model", nm = datasource)
    description <- "FAO Value Of Production information aggregated to magpie categories"
  }

  names(dimnames(out))[3] <- "scenario.model.variable"

  return(list(x = out,
              weight = NULL,
              unit = "million_USDMER05/yr",
              description = description))
}
