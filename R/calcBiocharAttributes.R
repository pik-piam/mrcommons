#' @title calcBiocharAttributes
#' @description Provides biochar attributes for different pyrolysis process conditions.
#' Currently covered are Dry Matter (DM), Carbon (C), and Generalizable Energy (GE).
#' Values are assembled from various literature sources.
#'
#' @param processCond Selects the pyrolysis process conditions for which data is returned.
#' Options: "all", "default", or subset of:
#' "SP-400", "SP-500", "SP-650", "SP-800", "FP-500", "P-Woolf", "P-Buffi", "P-KonTiki".
#'
#' @return List of magpie objects with results on global level, empty weight,
#' unit and description.
#'
#' @author Isabelle Weindl
#' @seealso [readPyrolysisConditions()]
#' @examples
#' \dontrun{
#' calcOutput("BiocharAttributes", processCond = "all")
#' }

calcBiocharAttributes <- function(processCond = "default") {

  # Mapping of internal names for pyrolysis process conditions to column headers
  # in source files
  processMapping <- list(
    "SP-400"    = "Slow pyrolysis 400 deg C",
    "SP-500"    = "Slow pyrolysis 500 deg C",
    "SP-650"    = "Slow pyrolysis 650 deg C",
    "SP-800"    = "Slow pyrolysis 800 deg C",
    "FP-500"    = "Fast pyrolysis 500 deg C",
    "P-Woolf"   = "Fischer-Tropsch - Woolf et al (2014)",
    "P-Buffi"   = "biopyrFuel - Buffi et al (2024) with H2",
    "P-KonTiki" = "KonTiki - Cornelissen et al (2016)"
  )

  allProcessConds <- names(processMapping)

  ## Define default subset if selected
  if (identical(processCond, "default")) {
    processCond <- c("SP-400", "SP-500", "SP-650", "SP-800", "FP-500", "P-Woolf", "P-KonTiki")
  }

  ## Read data sources
  sourceData <- list(
    schmidt     = readSource("PyrolysisConditions", subtype = "Schmidt_2019"),
    woolf       = readSource("PyrolysisConditions", subtype = "Woolf_2014"),
    buffi       = readSource("PyrolysisConditions", subtype = "Buffi_2024"),
    cornelissen = readSource("PyrolysisConditions", subtype = "Cornelissen_2016")
  )

  ## Helper function to extract attributes for one process
  extractAttributes <- function(attributeData, attributeMapping, pLabel) {
    values <- c(
      c  = as.numeric(attributeData[, , attributeMapping[["c"]]][, , processMapping[[pLabel]]]) / 100,
      ge = as.numeric(attributeData[, , attributeMapping[["ge"]]][, , processMapping[[pLabel]]]),
      dm = 1
    )

    att <- new.magpie("GLO", years = NULL, names = names(values), sets = c("region", "year", "attributes"))
    att <- add_dimension(att, dim = 3.2, add = "process_cond", nm = pLabel)

    att["GLO", , pLabel][, , names(values)] <- values

    return(att)
  }


  ## Fill magpie object
  out <- NULL

  for (p in allProcessConds) {

    # Determine data sources and mappings
    if (p %in% c("SP-400", "SP-500", "SP-650", "SP-800", "FP-500")) {
      attributeData <- sourceData$schmidt
      attributeMapping <- list(c = "C char (%)", ge = "LCV char (MJ/kg)")
    } else if (p == "P-Woolf") {
      attributeData <- sourceData$woolf
      attributeMapping <- list(c = "C char (%)", ge = "Energy content char (MJ/kg)")
    } else if (p == "P-Buffi") {
      attributeData <- sourceData$buffi
      attributeMapping <- list(c = "C char (%)", ge = "Energy content char (MJ/kg)")
    } else if (p == "P-KonTiki") {
      attributeData <- sourceData$cornelissen
      attributeMapping <- list(c = "C char (%)", ge = "Energy content char (MJ/kg)")
    }

    out <- mbind(out, extractAttributes(attributeData, attributeMapping, p))
  }

  ## Subset if not all process conditions were selected
  if (!identical(processCond, "all")) {
    out <- out[, , processCond]
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mt DM/Mt DM (dm), PJ/Mt DM (ge), Mt C/Mt DM (c)",
    description = "Biochar attributes for different pyrolysis process conditions"
  ))
}
