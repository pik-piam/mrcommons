#' Convert EU Reference Scenario
#'
#' Converts EU Reference Scenario magpie object into appropriate form for the REMIND model
#'
#' @author Renato Rodrigues, Falk Benke, Robin Hasse
#' @param x EU Reference Scenario magpie object derived from readEU_ReferenceScenario function
#' @param subtype data subtype. Either "techAssump.*", "2016" or "2020"
#' @return converted EU Reference Scenario magpie object
#' @examples
#' \dontrun{
#' test <- readSource("EU_ReferenceScenario", subtype = "2020", convert = TRUE)
#' }
#' @importFrom madrat toolGetMapping toolCountryFill toolAggregate
#'   toolCountry2isocode
#' @importFrom magclass getItems<- getItems getSets<- setItems mselect
#' @importFrom utils read.csv2
#' @export

convertEU_ReferenceScenario <- function(x, subtype) { # nolint: object_name_linter.

  eu27 <- c(
    "ALA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
    "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "GGY", "HUN", "IRL",
    "IMN", "ITA", "JEY", "LVA", "LTU", "LUX", "MLT", "NLD", "POL",
    "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
  )

  eu28 <- c(eu27, "GBR")

  # Technology assumptions -----------------------------------------------------

  if (grepl("^techAssump\\..+$", subtype)) {

    subsubtype <- sub("^techAssump\\.", "", subtype)

    # map to EU 28 countries
    mapping <- toolGetMapping(name = "regionmappingEU_ReferenceScenario.csv",
                              type = "regional",
                              where = "mappingfolder")
    mapping <- mapping[mapping[["CountryCode"]] %in% eu28, ]
    if (subsubtype %in% c("Domestic", "Renovation Costs")) {
      xReg <- x["EUR", , invert = TRUE]
      xReg <- toolAggregate(x = xReg,
                            rel = mapping,
                            from = gsub(" ", ".", subsubtype),
                            to = "CountryCode")
      if (subsubtype == "Renovation Costs") {
        x <- xReg
      } else {
        xEur <- mselect(x, region = "EUR")
        xEur <- do.call(mbind, lapply(eu28, setItems, x = xEur, dim = 1))
        x <- xEur
        x[!is.na(xReg)] <- xReg[!is.na(xReg)]
      }
    } else {
      x <- do.call(mbind, lapply(eu28, setItems, x = x, dim = 1))
      getSets(x)[1] <- "region"
    }

    # harmonise units
    getItems(x, "unit") <- sub("Euro", "EUR", getItems(x, "unit"))
    getItems(x, "unit") <- sub("^NA$", "EUR/appliance", getItems(x, "unit"))

  } else {

    # results ------------------------------------------------------------------

    x <- x["EL", , , invert = TRUE]
    getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))

  }

  x <- toolCountryFill(x, fill = NA, verbosity = 2)

  eu <- switch(subtype, "2016" = eu28, "2020" = eu27)

  xEu <- x[eu, , ]
  xEu[is.na(xEu)] <- 0
  x[eu, , ] <- xEu[eu, , ]

  return(x)
}
