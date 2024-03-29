
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
  ver <- "2023-12"

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

    # !!! ATTENTION !!!
    # Please update scenario names in calcMAgPIEReport.R

    fileList <- c("C_SDP_MC-Base-mag-4.mif",
                  "C_SDP_MC-NDC-mag-4.mif",
                  "C_SDP_MC-PkBudg650-mag-4.mif",
                  "C_SSP1-Base-mag-4.mif",
                  "C_SSP1-NDC-mag-4.mif",
                  "C_SSP1-PkBudg1050-mag-4.mif",
                  "C_SSP1-PkBudg650-mag-4.mif",
                  "C_SSP2EU-Base-mag-4.mif",
                  "C_SSP2EU-NDC-mag-4.mif",
                  "C_SSP2EU-PkBudg1050-mag-4.mif",
                  "C_SSP2EU-PkBudg650-mag-4.mif",
                  "C_SSP5-Base-mag-4.mif",
                  "C_SSP5-NDC-mag-4.mif",
                  "C_SSP5-PkBudg1050-mag-4.mif",
                  "C_SSP5-PkBudg650-mag-4.mif")

    x <- NULL
    for (f in fileList) {
      x <- mbind(x, read.report(file.path(ver, f), as.list = FALSE))
    }

  } else if (subtype == "supplyCurve_magpie_40") {
    regcode <- "_62eff8f7"

    # !!! ATTENTION !!!
    # Please update scenario names in mrremind::calcBiomassPrices.R if necessary

    scenarioNames <- c(
      "f30_bioen_price_SDP-MC-NDC-nocc_hist-NDC_replaced_flat",
      "f30_bioen_price_SDP-MC-NDC-nocc_hist-PkBudg650_replaced_flat",
      "f30_bioen_price_SDP-MC-NPI-nocc_hist-Base_replaced_flat",
      "f30_bioen_price_SSP1-NDC-nocc_hist-NDC_replaced_flat",
      "f30_bioen_price_SSP1-NDC-nocc_hist-PkBudg1050_replaced_flat",
      "f30_bioen_price_SSP1-NDC-nocc_hist-PkBudg650_replaced_flat",
      "f30_bioen_price_SSP1-NPI-nocc_hist-Base_replaced_flat",
      "f30_bioen_price_SSP2-NDC-nocc_hist-NDC_replaced_flat",
      "f30_bioen_price_SSP2-NDC-nocc_hist-PkBudg1050_replaced_flat",
      "f30_bioen_price_SSP2-NDC-nocc_hist-PkBudg650_replaced_flat",
      "f30_bioen_price_SSP2-NPI-nocc_hist-Base_replaced_flat",
      "f30_bioen_price_SSP5-NDC-nocc_hist-NDC_replaced_flat",
      "f30_bioen_price_SSP5-NDC-nocc_hist-PkBudg1050_replaced_flat",
      "f30_bioen_price_SSP5-NDC-nocc_hist-PkBudg650_replaced_flat",
      "f30_bioen_price_SSP5-NPI-nocc_hist-Base_replaced_flat"
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

  } else {
    stop("Not a valid subtype!")
  }
  return(x)
}
