#' @title Read in IPCC emissions
#'
#' @description Read in IPCC data: \itemize{ \item Read in IPCC emissions from livestock and
#' manure management. Source: IPCC Guidelines for National Greenhouse Gas
#' Inventories (2006); Chapter 10: Emissions from Livestock and Manure
#' Management.  \item Read in IPCC emissions from Lime and urea application.
#' Source: IPCC Guidelines for National Greenhouse Gas Inventories (2006);
#' Chapter 11: N2O Emissions from managed Soils and Co2 Emissions from Lime and
#' Urea Application.  \item Read in IPCC efficiency factors for burning of
#' residue. Source: IPCC Guidelines for Natinal Greenhouse Gas Inventories
#' (2006); Chapter 02: Generic Methodologies applicable to multiple Land-use
#' Categories. \item Read in soil related stock change factors for carbon and
#' manure parameterization. Source: IPCC Guidelines for National Greenhouse Gas
#' Inventories (2006); Chapter 5: Cropland.}
#'
#'
#' @param subtype data subtype
#' @return magpie object of the IPCC data
#' @author Nele Steinmetz, Stephen Wirth, Jan Philipp Dietrich,
#'         Kristine Karstens
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("IPCC", "awmsShr")
#' a <- readSource("IPCC", "awmsEfCh4")
#' a <- readSource("IPCC", "awmsParCh4")
#' a <- readSource("IPCC", "nExcrRate")
#' a <- readSource("IPCC", "awmsconfef3", convert = FALSE)
#' a <- readSource("IPCC", "fracgasms", convert = FALSE)
#' a <- readSource("IPCC", "fraclossms", convert = FALSE)
#' a <- readSource("IPCC", "emissionfactors", convert = FALSE)
#' a <- readSource("IPCC", "rescombusteff", convert = FALSE)
#' a <- readSource("IPCC", "efnsoil", convert = FALSE)
#' }
#' @importFrom reshape2 melt
readIPCC <- function(subtype) {

  # read in files
  files <- c(awmsShr          = "awmsShr.csv",
             awmsEfCh4        = "awmsEfCh4.csv",
             awmsParCh4       = "awmsParCh4.csv",
             nExcrRate        = "nExcrRate.csv",
             awmsconfef3      = "ch10_awms_conf_ef3.csv",
             fracgasms        = "ch10_Frac_GasMS.csv",
             fraclossms       = "ch10_Frac_LossMS.csv",
             emissionfactors  = "emission_factors.csv",
             rescombusteff    = "res_combust_eff.csv",
             efnsoil          = "ef_n_soil.csv",
             ch10_table10a9   = "ch10_table10a9.csv",
             SCF_input        = "ch5_F_I.csv",
             SCF_sub          = "ch5_F_LU.csv",
             SCF_sub2019      = "ch5_F_LU_2019.csv",
             SCF_LU           = "ch5_F_LU_raw.csv",
             SCF_LU2019       = "ch5_F_LU_2019_raw.csv",
             manure_table5p5c = "19R_V4_Ch05_Cropland_Table5p5C.csv",
             residues_table5p5b = "19R_V4_Ch05_Cropland_Table5p5B.csv")

  file <- toolSubtypeSelect(subtype, files)

  .read <- function(file) {
    .rowfilter <- function(x) return(!all(x == "" | is.na(x)))
    data <- read.csv(file, sep = ";", header = TRUE, comment.char = "*")
    colnames(data) <- gsub(".", "_", colnames(data), fixed = TRUE)
    data <- data[apply(data, 1, .rowfilter), ]
    regions <- c("groups" = "groups", "North_America" = "NOA",
                 "Western_Europe" = "WER", "Eastern_Europe" = "EER",
                 "Oceania" = "OCA", "Latin_America" = "LAM",
                 "Africa" = "AFR", "Middle_East" = "MDE", "Asia" = "ASI",
                 "Indian_Subcontinent" = "ISC")
    if (any(names(regions) %in% colnames(data))) {
      d <- as.magpie(data, spatial = "variable")
    } else {
      d <- as.magpie(data)
    }
    if (ncells(d) != 1 || getCells(d) != "GLO") {
      getCells(d) <- regions[getCells(d)]
    }
    getYears(d) <- "y2005"
    return(d)
  }

  if (subtype %in% c("awmsShr", "awmsEfCh4", "awmsParCh4", "nExcrRate")) {
    return(.read(file))
  }

  if (subtype %in% c("ch10_table10a9") || grepl("SCF", subtype)) {
    data <- read.csv(file, sep = ";", stringsAsFactors = FALSE,
                     comment.char = "*")
  } else if (subtype == "fraclossms") {
    data <-  read.csv(file, sep = ",", stringsAsFactors = FALSE,
                      header = TRUE, skip = 1)
  } else if (subtype == "efnsoil") {
    data <-  read.csv(file, sep = ",", stringsAsFactors = FALSE,
                      header = TRUE, skip = 3)
  } else if (subtype == "emissionfactors") {
    data <-  read.csv(file, sep = ",", stringsAsFactors = FALSE,
                      header = FALSE, skip = 2)
  } else if (subtype == "rescombusteff") {
    data <-  read.csv(file, sep = ",", stringsAsFactors = FALSE,
                      header = FALSE)
  } else {
    data <-  read.csv(file, sep = ",", stringsAsFactors = FALSE,
                      header = TRUE, comment.char = "*")
  }

  if (subtype == "awmsconfef3") {
    n <- data$awms
    value <- data[, 2]
    d <- new.magpie(years = "y2005", names = n,
                    sets = c("region", "years", "data"))
    d[, , ] <- value
    return(d)
  } else if (subtype %in% c("fracgasms", "fraclossms",
                            "efnsoil", "ch10_table10a9")) {
    molten <- melt(data, id.vars = "dummy")
    # create vector for variable names
    rows <- molten$dummy
    cols <- molten$variable
    n <- paste(rows, cols, sep = ".")
    # get values
    value <- molten$value
    d <- new.magpie(years = "y2005", names = n,
                    sets = c("region", "years", "data"))
    if (subtype == "efnsoil") getYears(d) <- "y2005"
    d[, , ] <- value
    return(d)
  } else if (subtype == "emissionfactors" || subtype == "rescombusteff") {
    d <- new.magpie(years = "y2005", names = data[, 1],
                    sets = c("region", "year", "data"))
    d[, , ] <- data[, 2]
    return(d)
  } else if (subtype == "manure_table5p5c") {
    d <- as.magpie(read.csv("19R_V4_Ch05_Cropland_Table5p5C.csv"))
    getSets(d) <- c("region", "year", "kli", "attributes")
    return(d)
  } else {
    d <- as.magpie(data)
    return(d)
  }
}
