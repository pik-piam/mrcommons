#' Read in PBL MAC curves from Harmsen_et_al_2019 for different subtypes
#'
#' @param subtype data subtype.
#' "ch4coal","ch4oil","ch4gas","ch4wstl","ch4wsts","ch4rice","ch4animals","ch4anmlwst",
#' "n2otrans","n2oadac","n2onitac","n2ofert","n2oanwst","n2owaste",
#' "HFC_tot", "SF6_tot", "PFC_tot" or "baseline_sources"
#' @return magpie object of the PBL_MACC_2019 data
#' @author Florian Humpenoeder
#' @seealso [madrat::readSource()]
#' @importFrom data.table as.data.table
#' @importFrom reshape2 dcast melt
#' @importFrom readxl read_xlsx
#' @importFrom magclass as.magpie
readPBL_MACC_2019 <- function(subtype) {   # nolint object_name_ linter   cyclocomp_linter

  readMMC1 <- function(sheet) {
    x <- as.data.table(read_xlsx("mmc1.xlsx", sheet = sheet))
    x <- melt(x, id.vars = c(1, 2), variable.name = "region")
    names(x) <- c("year", "steps", "region", "value")
    x$type <- sheet
    x$steps <- x$steps / 20 + 1
    x$year <- factor(x$year)
    x <- x[, c("region", "year", "type", "steps", "value")]
    x <- as.magpie(x, spatial = 1, temporal = 2, tidy = TRUE)
    names(dimnames(x))[3] <- "type.steps"
    return(x)
  }

  if (subtype == "ch4coal") {
    x <- readMMC1("CH4_coal")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "ch4oil") {
    x <- readMMC1("CH4_oilp")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "ch4gas") {
    x <- readMMC1("CH4_ngas")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "ch4wstl") {
    x <- readMMC1("CH4_landfills")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "ch4wsts") {
    x <- readMMC1("CH4_sewage")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "ch4rice") {
    x <- readMMC1("CH4_rice")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "ch4animals") {
    x <- readMMC1("CH4_ent fermentation")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "ch4anmlwst") {
    x <- readMMC1("CH4_manure")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "n2otrans") {
    x <- readMMC1("N2O_transport")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "n2oadac") {
    x <- readMMC1("N2O_adip acid")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "n2onitac") {
    x <- readMMC1("N2O_nitr acid")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "n2ofert") {
    x <- readMMC1("N2O_fertilizer")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "n2oanwst") {
    x <- readMMC1("N2O_manure")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "n2owaste") {
    x <- readMMC1("N2O_sewage")
    getNames(x, dim = "type") <- subtype
  }

  if (subtype == "SSP2_ch4coal") {
    x <- readMMC1("SSP2 CH4_coal")
    getNames(x, dim = "type") <- "ch4coal"
  }

  if (subtype == "SSP2_ch4oil") {
    x <- readMMC1("SSP2 CH4_oilp")
    getNames(x, dim = "type") <- "ch4oil"
  }

  if (subtype == "SSP2_ch4gas") {
    x <- readMMC1("SSP2 CH4_ngas")
    getNames(x, dim = "type") <- "ch4gas"
  }

  if (subtype == "SSP2_ch4wstl") {
    x <- readMMC1("SSP2 CH4_landfills")
    getNames(x, dim = "type") <- "ch4wstl"
  }

  if (subtype == "SSP2_ch4wsts") {
    x <- readMMC1("SSP2 CH4_sewage")
    getNames(x, dim = "type") <- "ch4wsts"
  }

  if (subtype == "SSP2_ch4rice") {
    x <- readMMC1("SSP2 CH4_rice")
    getNames(x, dim = "type") <- "ch4rice"
  }

  if (subtype == "SSP2_ch4animals") {
    x <- readMMC1("SSP2 CH4_ent fermentation")
    getNames(x, dim = "type") <- "ch4animals"
  }

  if (subtype == "SSP2_ch4anmlwst") {
    x <- readMMC1("SSP2 CH4_manure")
    getNames(x, dim = "type") <- "ch4anmlwst"
  }

  if (subtype == "SSP2_n2otrans") {
    x <- readMMC1("SSP2 N2O_transport")
    getNames(x, dim = "type") <- "n2otrans"
  }

  if (subtype == "SSP2_n2oadac") {
    x <- readMMC1("SSP2 N2O_adip acid")
    getNames(x, dim = "type") <- "n2oadac"
  }

  if (subtype == "SSP2_n2onitac") {
    x <- readMMC1("SSP2 N2O_nitr acid")
    getNames(x, dim = "type") <- "n2onitac"
  }

  if (subtype == "SSP2_n2ofert") {
    x <- readMMC1("SSP2 N2O_fertilizer")
    getNames(x, dim = "type") <- "n2ofert"
  }

  if (subtype == "SSP2_n2oanwst") {
    x <- readMMC1("SSP2 N2O_manure")
    getNames(x, dim = "type") <- "n2oanwst"
  }

  if (subtype == "SSP2_n2owaste") {
    x <- readMMC1("SSP2 N2O_sewage")
    getNames(x, dim = "type") <- "n2owaste"
  }

  if (subtype == "HFC_tot") {
    stop("not working yet")
  }

  if (subtype == "SF6_tot") {
    stop("not working yet")
  }

  if (subtype == "PFC_tot") {
    stop("not working yet")
  }

  if (subtype == "baseline_sources") {
    x <- as.data.table(read_xlsx("mmc1.xlsx", sheet = "SSP2 CH4 N2O baseline emissions"))
    x <- melt(x, id.vars = c(1, 2), variable.name = "type")
    names(x) <- c("year", "region", "type", "value")
    levels(x$type)[1] <- "ch4coal"
    levels(x$type)[2] <- "ch4oil"
    levels(x$type)[3] <- "ch4gas"
    levels(x$type)[4] <- "ch4wstl"
    levels(x$type)[5] <- "ch4wsts"
    levels(x$type)[6] <- "ch4rice"
    levels(x$type)[7] <- "ch4animals"
    levels(x$type)[8] <- "ch4anmlwst"
    levels(x$type)[9] <- "n2otrans"
    levels(x$type)[10] <- "n2oadac"
    levels(x$type)[11] <- "n2onitac"
    levels(x$type)[12] <- "n2ofert"
    levels(x$type)[13] <- "n2oanwst"
    levels(x$type)[14] <- "n2owaste"

    x$year <- factor(x$year)
    x <- dcast(x, region + year ~ type, value.var = "value")
    x <- as.magpie(x, spatial = 1, temporal = 2)
    names(dimnames(x))[3] <- "type"
  }

  return(x)
}
