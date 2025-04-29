#' Read in PBL MAC curves from Harmsen_et_al_2022 for different subtypes and subsets, using the
#' baseline-dependent IMAGE SSP2 version
#'
#' @param subtype data subtype.
#' "ch4coal","ch4oil","ch4gas","ch4wstl","ch4wsts","ch4rice","ch4animals","ch4anmlwst",
#' "n2otrans","n2oadac","n2onitac","n2ofert","n2oanwst","n2owaste"
#' @param subset data subset.
#' "Default", "Optimistic", "Pessimistic"
#' @return magpie object of the PBL_MACC_SSP2_2022 data
#' @author Michael Windisch, Florian Humpenoeder, Gabriel Abrahao
#' @seealso [madrat::readSource()]
#' @importFrom data.table as.data.table
#' @importFrom reshape2 dcast melt
#' @importFrom readxl read_xlsx
#' @importFrom magclass as.magpie
readPBL_MACC_SSP2_2022 <- function(subtype, subset) { # nolint

  readMMC1 <- function(sub, scen) {
    sheetname <- paste0("SSP2 ", sub, "_", scen)
    sheetname <- substring(sheetname, 1, 31)
    x <- as.data.table(read_xlsx("Data_MAC_CH4N2O_Harmsen et al_2023_PBL.xlsx", sheet = sheetname))
    # Sometimes the sheet has one or more garbage comment rows before the data, here we check
    # if the first column is the year as expected and if not retry reading by skipping up to 10 lines
    iskip <- 0
    while (!(colnames(x)[1] %in% c("t", "year", "period")) || iskip >= 10) {
      iskip <- iskip + 1
      x <- as.data.table(read_xlsx("Data_MAC_CH4N2O_Harmsen et al_2023_PBL.xlsx",
                                   sheet = paste0("SSP2 ", sub, "_", scen), skip = iskip))
    }
    x <- melt(x, id.vars = c(1, 2), variable.name = "region")
    names(x) <- c("year", "steps", "region", "value")
    x$type <- sub
    x$scen <- scen
    # x$steps <- x$steps/20+1 # SSP2 file from Mathijs comes in steps, not with specified prices # nolint
    x$year <- factor(x$year)
    x <- x[, c("region", "year", "type", "scen", "steps", "value")]
    x <- as.magpie(x, spatial = 1, temporal = 2, tidy = TRUE)
    names(dimnames(x))[3] <- "type.scen.steps"
    return(x)
  }

  readIMAGESSP2Baseline <- function() {
    inimage <- read_xlsx("Data_MAC_CH4N2O_Harmsen et al_2023_PBL.xlsx", sheet = "SSP2_CH4 N2O_baseline emissions")
    names(inimage) <- c(
      "year", "region",
      "ch4coal", "ch4oil", "ch4gas", "ch4wstl", "ch4wsts", "ch4rice", "ch4animals",
      "ch4anmlwst", "n2otrans", "n2oadac", "n2onitac", "n2ofert", "n2oanwst", "n2owaste"
    )
    tidyimage <- melt(inimage, id.vars = c(1, 2), variable.name = "type")
    baseimage <- as.magpie(tidyimage, spatial = 2, temporal = 1, tidy = TRUE)
    return(baseimage)
  }

  if (subtype == "ch4coal") {
    x <- readMMC1("CH4_coal", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4oil") {
    x <- readMMC1("CH4_oilp", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4gas") {
    x <- readMMC1("CH4_ngas", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4wstl") {
    x <- readMMC1("CH4_landfills", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4wsts") {
    x <- readMMC1("CH4_sewage", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4rice") {
    x <- readMMC1("CH4_rice", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4animals") {
    x <- readMMC1("CH4_ent fermentation", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4anmlwst") {
    x <- readMMC1("CH4_manure", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2otrans") {
    x <- readMMC1("N2O_transport", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2oadac") {
    x <- readMMC1("N2O_adip acid", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2onitac") {
    # Try different names used in the Excel sheet
    possiblenames <- c("N2O_nitric acid", "N2O_nitr acid")
    for (tryname in possiblenames) {
      x <- tryCatch(
        {
          readMMC1(tryname, subset)
        },
        error = function(e) {
          return(e)
        }
      )
      if (!"error" %in% class(x)) {
        break
      }
    }
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2ofert") {
    x <- readMMC1("N2O_fertilizer", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2oanwst") {
    x <- readMMC1("N2O_manure", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2owaste") {
    x <- readMMC1("N2O_sewage", subset)
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "IMAGESSP2Baseline") {
    x <- readIMAGESSP2Baseline()
  }

  return(x)
}
