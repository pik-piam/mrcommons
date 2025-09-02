#' @title calcBiocharEfficiency
#' @description Calculates biochar conversion efficiencies for different pyrolysis
#' process conditions, using data from multiple literature sources.
#' Yield metrics are expressed as output biochar per biomass input on dry matter,
#' carbon, and energy basis. Output can also cover H/C ratio when available.
#'
#' @param processCond Selects the pyrolysis process conditions for which data is returned.
#' Options: "all", "default", or subset of:
#' "SP-400", "SP-500", "SP-650", "SP-800", "FP-500", "P-Woolf", "P-Buffi", "P-KonTiki".
#' @param outputType Selects the output metric for which data is returned.
#' Options: "all", "default", or subset of:
#' "dm_yield", "en_yield", "c_yield", "HC_ratio".
#'
#' @return List of magpie objects with results on global level, empty weight,
#' unit and description.
#'
#' @author Isabelle Weindl
#' @seealso [readPyrolysisConditions()]
#' @examples
#' \dontrun{
#' calcOutput("BiocharEfficiency", processCond = "all", outputType = "all")
#' }

calcBiocharEfficiency <- function(processCond = "default", outputType = "default") {

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

  ## Define default subsets if selected
  if (identical(processCond, "default")) {
    processCond <- c("SP-400", "SP-500", "SP-650", "SP-800", "FP-500", "P-Woolf", "P-KonTiki")
  }
  if (identical(outputType, "default")) {
    outputType <- c("dm_yield", "en_yield", "c_yield")
  }

  ## Read data sources
  sourceData <- list(
    schmidt     = readSource("PyrolysisConditions", subtype = "Schmidt_2019"),
    woolf       = readSource("PyrolysisConditions", subtype = "Woolf_2014"),
    buffi       = readSource("PyrolysisConditions", subtype = "Buffi_2024"),
    cornelissen = readSource("PyrolysisConditions", subtype = "Cornelissen_2016")
  )

  ## Read biomass attributes (betr + begr)
  bioatt <- calcOutput("Attributes", aggregate = FALSE)[, , c("betr", "begr")]

  ## Helper function to extract all efficiency values for one process
  extractEfficiency <- function(attributeData, pLabel, sourceType) {
    values <- list(dm_yield = NA, en_yield = NA, c_yield = NA, HC_ratio = NA)

    processName <- processMapping[[pLabel]]

    if (sourceType == "Schmidt") {
      enChar  <- as.numeric(attributeData[, , "LCV char (MJ/kg)"][, , processName])
      enBio  <- as.numeric(attributeData[, , "LCV biomass (MJ/kg wm)"][, , processName])
      dmBio  <- (100 - as.numeric(attributeData[, , "Initial moisture (% wm)"][, , processName])) / 100
      wmYield <- as.numeric(attributeData[, , "Char yield (% wm)"][, , processName]) / 100

      values$c_yield   <- as.numeric(attributeData[, , "C in char (%)"][, , processName]) / 100
      values$en_yield  <- (wmYield * enChar) / enBio
      values$dm_yield  <- wmYield / dmBio
      values$HC_ratio  <- as.numeric(attributeData[, , "H/C ratio (-)"][, , processName])

    } else if (sourceType == "Woolf" || sourceType == "Buffi" || sourceType == "Cornelissen") {
      enChar           <- as.numeric(attributeData[, , "Energy content char (MJ/kg)"][, , processName])
      enBiomass        <- as.numeric(bioatt[, , "betr"][, , "ge"])

      values$en_yield  <- as.numeric(attributeData[, , "Energy yield (MJ/MJ)"][, , processName])
      values$c_yield   <- as.numeric(attributeData[, , "Carbon yield (kg/kg)"][, , processName])
      values$dm_yield  <- (values$en_yield * enBiomass) / enChar
      values$HC_ratio  <- NA
    }

    eff <- new.magpie("GLO", years = NULL, names = names(values), sets = c("region", "year", "efficiency"))
    eff <- add_dimension(eff, dim = 3.2, add = "process_cond", nm = pLabel)

    eff["GLO", , pLabel][, , names(values)] <- unlist(values)

    return(eff)
  }


  ## Fill magpie object
  out <- NULL

  for (p in allProcessConds) {

    # Determine data sources and types
    if (p %in% c("SP-400", "SP-500", "SP-650", "SP-800", "FP-500")) {
      attributeData <- sourceData$schmidt
      sourceType <- "Schmidt"
    } else if (p == "P-Woolf") {
      attributeData <- sourceData$woolf
      sourceType <- "Woolf"
    } else if (p == "P-Buffi") {
      attributeData <- sourceData$buffi
      sourceType <- "Buffi"
    } else if (p == "P-KonTiki") {
      attributeData <- sourceData$cornelissen
      sourceType <- "Cornelissen"
    }

    out <- mbind(out, extractEfficiency(attributeData, p, sourceType))
  }

  ## Subset if not all process conditions or output types were selected
  if (!identical(processCond, "all")) {
    out <- out[, , processCond]
  }
  if (!identical(outputType, "all")) {
    out <- out[, , outputType]
  }


  return(list(
    x = out,
    weight = NULL,
    unit = "dimensionless",
    description = "Biochar yields per unit biomass on the basis of mass (dry matter), energy, and carbon"
  ))
}
