
#' Read MAgPIE data
#'
#' Read-in MAgPIE data
#'
#' @param subtype Either "EmiAirPoll", "macBase" or "co2tax"
#' @importFrom madrat regionscode
#' @return magpie object
#' @author Julian Oeser
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource(type = "MAgPIE", subtype = "EmiAPExo")
#' }
#'
readMAgPIE <- function(subtype) {

  # input data version
  ver <- "2026-05"

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

    fileList <- c("C_SSP1-NPi2025_2026-05-20_03.41.57-mag-4.mif",     # ssp1.rcp45
                  "C_SSP1-PkBudg1000_2026-05-20_03.44.55-mag-4.mif",  # ssp1.rcp26
                  "C_SSP1-PkBudg750_2026-05-20_03.43.26-mag-4.mif",   # ssp1.rcp20
                  "C_SSP2-NDC_2026-05-20_03.30.02-mag-4.mif",         # ssp2.rcp37
                  "C_SSP2-NPi2025_2026-05-19_19.50.08-mag-4.mif",     # ssp2.rcp45  --> ssp5.rcp45
                  "C_SSP2-PkBudg1000_2026-05-20_03.34.31-mag-4.mif",  # ssp2.rcp26  --> ssp5.rcp26
                  "C_SSP2-PkBudg750_2026-05-20_03.31.32-mag-4.mif",   # ssp2.rcp20  --> ssp5.rcp20
                  #"C_SSP2-PkBudg750_wo100EJBiobound_2026-05-20_03.33.02-mag-4.mif", # cannot be used because it has the same rcp as SSP2-PkBudg750 (rcp20)
                  #"C_SSP2-EcBudg500_2026-05-20_03.36.01-mag-4.mif",                 # cannot be used because it has the same rcp as SSP2-PkBudg1000 (rcp26)
                  "C_SSP3-NPi2025_2026-05-20_03.37.30-mag-4.mif",     #ssp3.rcp45
                  "C_SSP3-PkBudg1000_2026-05-20_03.38.59-mag-4.mif"   #ssp3.rcp26
                  #"C_SSP3-rollBack_2026-05-20_03.40.28-mag-4.mif"                   # cannot be used because it has the same rcp as SSP3-NPi2025 (rcp45)
                  )

    x <- NULL
    for (f in fileList) {
      x <- mbind(x, read.report(file.path(ver, f), as.list = FALSE))
    }

  } else if (subtype == "supplyCurve_magpie_40") {
    regcode <- "_62eff8f7"

    # !!! ATTENTION !!!
    # Please update scenario names in mrremind::calcBiomassPrices.R if necessary

    scenarioNames <- c(
      "f30_bioen_price_SDP-MC-SSP1-NPi_replaced_flat",
      "f30_bioen_price_SDP-MC-SSP1-PkBudg650_replaced_flat",
      "f30_bioen_price_SSP2-SSP2_lowEn-NPi_replaced_flat",
      "f30_bioen_price_SSP2-SSP2_lowEn-PkBudg1000_replaced_flat",
      "f30_bioen_price_SSP2-SSP2_lowEn-PkBudg650_replaced_flat",
      "f30_bioen_price_SSP2-SSP2-NPi_replaced_flat",
      "f30_bioen_price_SSP2-SSP2-PkBudg1000_replaced_flat",
      "f30_bioen_price_SSP2-SSP2-PkBudg650_replaced_flat",
      "f30_bioen_price_SSP3-SSP2-NPi_replaced_flat",
      "f30_bioen_price_SSP3-SSP2-PkBudg1000_replaced_flat",
      "f30_bioen_price_SSP3-SSP2-PkBudg650_replaced_flat",
      "f30_bioen_price_SSP5-SSP5-NPi_replaced_flat",
      "f30_bioen_price_SSP5-SSP5-PkBudg1000_replaced_flat",
      "f30_bioen_price_SSP5-SSP5-PkBudg650_replaced_flat"
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
