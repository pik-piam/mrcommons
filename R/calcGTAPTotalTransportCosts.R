#' @title calcGTAPTotalTransportCosts
#' @description Calculates iso-level total global transport costs from GTAP for GTAP food commodities
#' @return List of magpie object with results on country level, unit and description.
#' @author David M Chen
#' @examples
#' \dontrun{
#' calcOutput("GTAPTotalTransportCosts")
#' }
#'
calcGTAPTotalTransportCosts <- function() {

nvfa <- readSource("GTAP81", subtype = "SF01")

gtapFoods <- c("pdr", "wht", "gro", "v_f", "osd", "c_b", "pfb", "ocr", "ctl", "oap", "rmk", "wol", "frs", "fsh")

transpInputs <- c("trd", "otp", "wtp", "atp")

nonTranspInputs <- c("pdr", "wht", "gro", "v_f", "osd", "c_b", "pfb",
                       "ocr", "ctl", "oap", "rmk", "wol", "frs", "fsh",
                       "coa", "oil", "gas", "omn", "cmt", "omt", "vol",
                       "mil", "pcr", "sgr", "ofd", "b_t", "tex", "wap",
                       "lea", "lum", "ppp", "p_c", "crp", "nmm", "i_s",
                       "nfm", "fmp")

# sum imported and domestic inputs - very little imported transport inputs
nvfa <- dimSums(nvfa, dim = 3.3, na.rm = TRUE)

# subset to market expenditure (no taxes) and GTAP foods
nvfa  <- collapseNames(nvfa[, , "mktexp"][, , list("PROD_COMM" = gtapFoods)])

tcostInputs <- dimSums(nvfa[, , list("DEMD_COMM" = transpInputs)], dim = 3.1)
nonTcostInputs <-  dimSums(nvfa[, , list("DEMD_COMM" = nonTranspInputs)], dim = 3.1)

tcostPerUnitInput <- tcostInputs / nonTcostInputs

tcostToSecondary <- tcostPerUnitInput * nvfa[, , list("DEMD_COMM" = gtapFoods)]
tcostToSecondary <- dimSums(tcostToSecondary, dim = "DEMD_COMM")

# half of transport of inputs to market and half of transport from market to consumer
totalTcosts <- (tcostInputs + tcostToSecondary) / 2

return(list(x = totalTcosts,
            weight = NULL,
            unit = "million current US$MER/yr",
            description = "Transport costs of GTAP commodities (half of input to market and market to value"))
}
