#' @title calcGTAPTotalTransportCosts
#' @description Calculates iso-level total global transport costs from GTAP for GTAP food commodities
#' @return List of magpie object with results on country level, unit and description.
#' @author David M Chen
#' @param costType transport or wholesale
#' @param version "81" or "9"
#' @examples
#' \dontrun{
#' calcOutput("GTAPTotalTransportCosts")
#' }
#'
calcGTAPTotalTransportCosts <- function(costType = "transport", version = "9") {

  nvfa <- readSource("GTAPv8v9", subtype = paste0(version, ":SF01"))

  gtapFoods <- c("pdr", "wht", "gro", "v_f", "osd", "c_b", "pfb", "ocr", "ctl", "oap", "rmk", "wol", "frs", "fsh",
                 "pcr", "sgr", "vol", "mil", "cmt", "omt", "b_t")

  if (costType == "transport") {
    transpInputs <-  c("otp", "wtp", "atp")
  } else if (costType == "wholesale") {
    transpInputs <- "trd"
  }

  nonTranspInputs <- c("pdr", "wht", "gro", "v_f", "osd", "c_b", "pfb",
                       "ocr", "ctl", "oap", "rmk", "wol", "frs", "fsh",
                       "coa", "oil", "gas", "omn", "cmt", "omt", "vol",
                       "mil", "pcr", "sgr", "ofd", "b_t", "tex", "wap",
                       "lea", "lum", "ppp", "p_c", "crp", "nmm", "i_s",
                       "nfm", "fmp", "trd")

  # sum imported and domestic inputs - very little imported transport inputs
  nvfa <- dimSums(nvfa, dim = 3.3, na.rm = TRUE)

  # subset to market expenditure (no taxes) and GTAP foods
  nvfa  <- collapseNames(nvfa[, , "mktexp"][, , list("prod_comm" = gtapFoods)])

  tcostInputs <- dimSums(nvfa[, , list("demd_comm" = transpInputs)], dim = 3.1)
  nonTcostInputs <-  dimSums(nvfa[, , list("demd_comm" = nonTranspInputs)], dim = 3.1)

  tcostPerUnitInput <- tcostInputs / nonTcostInputs

  tcostToSecondary <- tcostPerUnitInput * nvfa[, , list("demd_comm" = gtapFoods)]
  tcostToSecondary <- dimSums(tcostToSecondary, dim = "demd_comm")

  # half of transport of inputs to market and half of transport from market to consumer
  totalTcosts <- (tcostInputs + tcostToSecondary) / 2

  totalTcosts <-  GDPuc::convertGDP(totalTcosts, unit_in = "current US$MER",
                                    unit_out = "constant 2017 US$MER",
                                    replace_NAs = "no_conversion")

  return(list(x = totalTcosts,
              weight = NULL,
              unit = "million constant 2017 US$MER/yr",
              description = "Costs for GTAP commodities (half of input to market and market to value"))
}
