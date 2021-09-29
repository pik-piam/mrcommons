
calcRatioPPP2MER <- function(RatioPPP2MER = "SSP") {

  if (RatioPPP2MER == "SSP") {
    data <- readSource("SSP", subtype = "ratioPM")
  } else if (RatioPPP2MER == "OECD") {
    data <- readSource("OECD", subtype = "ratioPM")
  } else {
    stop("Bad input for calcRatioPPP2MER. Invalid 'RatioPPP2MER' argument.")
  }

  getNames(data) <- NULL

  weight <- calcOutput("GDPppp", aggregate = FALSE)[, 2005, "gdp_SSP2"] 

  return(list(x = data,
              weight = weight,
              unit = "-",
              description = glue::glue("Ratio of GDP in constant 2005 Int$PPP over GDP \\
                                        in constant 2005 US$MER (source: {RatioPPP2MER}).")))
}
