
#' Read MAgPIE data
#'
#' Read-in MAgPIE data
#'
#' @param subtype Either "EmiAirPoll", "macBase" or "co2tax"
#' @importFrom madrat regionscode
#' @return magpie object
#' @author Julian Oeser
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource(type = "MAgPIE", subtype = "EmiAPExo")
#' }
#'
readMAgPIE <- function(subtype) {

  # input data version
  ver <- "2023-05"

  if (subtype == "EmiAirPoll") {
    x <- read.csv(file.path(ver, "emiAPexo.csv"), row.names = 1)
    # reorder the data frame
    x$var   <- x$value
    x$value <- NULL
    # convert into a magpie object
    x <- as.magpie(x, datacol = 6)

  } else if (subtype == "macBase") {
    x <- read.csv(file.path(ver, "macBaseMagpie.csv"), row.names = 1)
    # reorder the data frame
    x$var   <- x$value
    x$value <- NULL
    # convert into a magpie object
    x <- as.magpie(x, datacol = 6)

  } else if (subtype == "macBaseCO2luc") {
    x <- read.magpie(file.path(ver, "p_macBaseMagpie_co2luc_SSP2.cs4r"))

  } else if (subtype == "co2tax") {
    x <- read.magpie(file.path(ver, "p_magpietax200.cs4r"))

  } else if (subtype == "abatparam_co2") {
    x <- read.magpie(file.path(ver, "p_abatparam_CO2.cs4r"))

  } else if (subtype == "MAgPIEReport_extensive") {

    # last version before the current /p/tmp/aloisdir/magpie/output
    # current version /p/projects/piam/runs/coupled-magpie/output

    # !!! ATTENTION !!!
    # Please update scenario names in calcMAgPIEReport.R

    fileList <- c("C_SDP_MC-Base-mag-4.mif",
                  "C_SDP_MC-NDC-mag-4.mif",
                  "C_SDP_MC-PkBudg500-mag-4.mif",
                  "C_SSP1-Base-mag-4.mif",
                  "C_SSP1-NDC-mag-4.mif",
                  "C_SSP1-PkBudg1150-mag-4.mif",
                  "C_SSP1-PkBudg500-mag-4.mif",
                  "C_SSP2EU-Base-mag-4.mif",
                  "C_SSP2EU-NDC-mag-4.mif",
                  "C_SSP2EU-PkBudg1150-mag-4.mif",
                  "C_SSP2EU-PkBudg500-mag-4.mif",
                  "C_SSP5-Base-mag-4.mif",
                  "C_SSP5-NDC-mag-4.mif",
                  "C_SSP5-PkBudg1150-mag-4.mif",
                  "C_SSP5-PkBudg500-mag-4.mif")

    x <- NULL
    for (f in fileList) {
      x <- mbind(x, read.report(file.path(ver, f), as.list = FALSE))
    }

  } else if (subtype == "supplyCurve_magpie_40") {
    regcode <- "690d3718e151be1b450b394c1064b1c5"

    # !!! ATTENTION !!!
    # Please update scenario names in calcBiomassPrice.R if necessary

    scenarioNames <- c("f30_bioen_price_SDP-NDC-NDC_replaced_flat_",
                        "f30_bioen_price_SDP-NDC-PkBudg1300_replaced_flat_",
                        "f30_bioen_price_SDP-NDC-PkBudg900_replaced_flat_",
                        "f30_bioen_price_SDP-NPI-Base_replaced_flat_",
                        "f30_bioen_price_SSP1-NDC-NDC_",
                        "f30_bioen_price_SSP1-NDC-PkBudg1300_replaced_flat_",
                        "f30_bioen_price_SSP1-NDC-PkBudg900_",
                        "f30_bioen_price_SSP1-NPI-Base_",
                        "f30_bioen_price_SSP2-NDC-NDC_",
                        "f30_bioen_price_SSP2-NDC-PkBudg1300_",
                        "f30_bioen_price_SSP2-NDC-PkBudg900_replaced_flat_",
                        "f30_bioen_price_SSP2-NPI-Base_",
                        "f30_bioen_price_SSP5-NDC-NDC_replaced_flat_",
                        "f30_bioen_price_SSP5-NDC-PkBudg1300_replaced_flat_",
                        "f30_bioen_price_SSP5-NDC-PkBudg900_replaced_flat_",
                        "f30_bioen_price_SSP5-NPI-Base_replaced_flat_",
                        "f30_bioen_price_SSP2-NDC-nocc-NDC_replaced_flat_",
                        "f30_bioen_price_SSP2-NPI-nocc-Base_replaced_flat_",
                        "f30_bioen_price_SSP2-NPI-nocc-NPI_replaced_flat_",
                        "f30_bioen_price_SSP2-NDC-nocc-PkBudg500_replaced_flat_",
                        "f30_bioen_price_SSP2-NDC-nocc-PkBudg1150_replaced_flat_"
                        )


    fileList <- file.path(ver, paste0(scenarioNames, regcode, ".cs4r"))
    setnames  <- c("region", "year", "scenario", "char")

    if (!all(file.exists(fileList))) {
      vcat(1, "Could not find ", fileList[!file.exists(fileList)], "\n")
    }

    x <- NULL
    for (f in fileList) {
      x <- mbind(x, read.magpie(f))
    }
    getSets(x) <- setnames

    # make SSP2EU scenario using SSP2 data --- ATTENTION: needs to be deleted as soon as we have data for SSP2EU
    xSSP2EU <- x[, , c("SSP2-NDC-NDC", "SSP2-NDC-PkBudg1300", "SSP2-NDC-PkBudg900", "SSP2-NPI-Base")]
    getNames(xSSP2EU) <- gsub("SSP2", "SSP2EU", getNames(xSSP2EU))
    x <- mbind(x, xSSP2EU)
    # make SDP* scenarios using SSP1 data --- ATTENTION: needs to be deleted as soon as we have data for SDP*
    xSDPEI <- x[, , c("SSP1-NDC-NDC", "SSP1-NDC-PkBudg1300", "SSP1-NDC-PkBudg900", "SSP1-NPI-Base")]
    getNames(xSDPEI) <- gsub("SSP1", "SDP_EI", getNames(xSDPEI))
    xSDPRC <- x[, , c("SSP1-NDC-NDC", "SSP1-NDC-PkBudg1300", "SSP1-NDC-PkBudg900", "SSP1-NPI-Base")]
    getNames(xSDPRC) <- gsub("SSP1", "SDP_RC", getNames(xSDPRC))
    xSDPMC <- x[, , c("SSP1-NDC-NDC", "SSP1-NDC-PkBudg1300", "SSP1-NDC-PkBudg900", "SSP1-NPI-Base")]
    getNames(xSDPMC) <- gsub("SSP1", "SDP_MC", getNames(xSDPMC))
    x <- mbind(x, xSDPEI, xSDPRC, xSDPMC)

  } else {
    stop("Not a valid subtype!")
  }
  return(x)
}
