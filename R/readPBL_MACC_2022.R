#' Read in PBL MAC curves from Harmsen_et_al_2022 for different subtypes and subsets
#'
#' @param subtype data subtype.
#' "ch4coal","ch4oil","ch4gas","ch4wstl","ch4wsts","ch4rice","ch4animals","ch4anmlwst",
#' "n2otrans","n2oadac","n2onitac","n2ofert","n2oanwst","n2owaste"
#' @param subset data subset.
#' "Default", "Optimistic", "Pessimistic"
#' @return magpie object of the PBL_MACC_2022 data
#' @author Michael Windisch, Florian Humpenoeder
#' @seealso [readSource()]
#' @importFrom data.table as.data.table
#' @importFrom reshape2 dcast melt
#' @importFrom readxl read_xlsx
#' @importFrom magclass as.magpie
#' @importFrom methods new

readPBL_MACC_2022 <- function(subtype, subset) { # nolint

  readMMC1 <- function(sub, scen) {
    x <- as.data.table(read_xlsx("Data_MAC_CH4N2O_Harmsen et al_PBL.xlsx", sheet = paste0(sub, scen)))
    x <- melt(x, id.vars = c(1, 2), variable.name = "region")
    names(x) <- c("year", "steps", "region", "value")
    x$type <- sub
    x$scen <- scen
    x$year <- factor(x$year)
    x <- x[, c("region", "year", "type", "scen", "steps", "value")]
    x <- x[!is.na(x$year), ]
    nsteps <- nrow(x[x$year == x$year[1] & x$region == x$region[1], ])
    x$steps <- seq(1, nsteps, by = 1)
    x <- as.magpie(x, spatial = 1, temporal = 2, tidy = TRUE)
    names(dimnames(x))[3] <- "type.scen.steps"
    return(x)
  }

  readIMAGEGlobalEmissionFactors <- function() {
    inimage <- read_xlsx("Data_MAC_CH4N2O_Harmsen et al_PBL.xlsx", sheet = "Global_mean_EFs_2015_IMAGE", range = "C6:C20")[[1]]
    gefsimage <- new("magpie", .Data = structure(inimage, 
    .Dim = c(1L, 1L, 14L), .Dimnames = list(region = NULL, year = NULL, 
    type = c(
        "ch4coal","ch4oil","ch4gas","ch4wstl","ch4wsts","ch4rice","ch4animals","ch4anmlwst","n2otrans","n2oadac","n2onitac","n2ofert","n2oanwst","n2owaste"
    ))))
    return(gefsimage)
  }


  if (subtype == "ch4coal") {
    x <- readMMC1("CH4_coal", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4oil") {
    x <- readMMC1("CH4_oilp", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4gas") {
    x <- readMMC1("CH4_ngas", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4wstl") {
    x <- readMMC1("CH4_land", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4wsts") {
    x <- readMMC1("CH4_sewa", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4rice") {
    x <- readMMC1("CH4_rice", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4animals") {
    x <- readMMC1("CH4_entf", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "ch4anmlwst") {
    x <- readMMC1("CH4_manu", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2otrans") {
    x <- readMMC1("N2O_tran", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2oadac") {
    x <- readMMC1("N2O_adip", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2onitac") {
    x <- readMMC1("N2O_nitr", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2ofert") {
    x <- readMMC1("N2O_fert", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2oanwst") {
    x <- readMMC1("N2O_manu", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if (subtype == "n2owaste") {
    x <- readMMC1("N2O_sewa", paste0("_", substr(subset, 1, 1)))
    getNames(x, dim = "type") <- subtype
    getNames(x, dim = "scen") <- subset
  }

  if(subtype=="IMAGEGlobalEmissionFactors") {
    x <- readIMAGEGlobalEmissionFactors()
  }

  return(x)
}
