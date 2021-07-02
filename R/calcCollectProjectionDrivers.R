#' @importFrom magclass getNames<-


calcCollectProjectionDrivers <- function(driver = "all") {

  if (driver != "all") {
    all <- calcOutput("CollectProjectionDrivers", aggregate = FALSE)
    combined <- collapseNames(all[, , driver])
  } else {

    ## add education indicators

    popSSPsres <- calcOutput("Population", aggregate = FALSE, naming = "indicator.scenario")
    gdpSSPsres <- calcOutput("GDPppp", aggregate = FALSE, naming = "indicator.scenario")
    gdpSSPsresMER <- calcOutput("GDPppp", GDPpppPast = "IHME_USD05_MER_pc_completed",
                                GDPpppFuture = "SRES_SSP_completed", GDPpppCalib = "past", aggregate = FALSE,
                                naming = "indicator.scenario")

    getNames(gdpSSPsresMER, dim = 1) <- "gdpmer"

    urbanShrSSP <- calcOutput("Urban", UrbanCalib = "past", UrbanPast = "WDI", UrbanFuture = "SSP",
                                aggregate = FALSE, naming = "indicator.scenario")
    urbanSSP <- urbanShrSSP * popSSPsres[, , getNames(urbanShrSSP)]
    getNames(urbanSSP, dim = 1) <- "urban"

    combined <- mbind(
      popSSPsres,
      gdpSSPsres,
      gdpSSPsresMER,
      urbanSSP
    )

  }



  if (driver == "gdp") {
    unit <- "Mio USD 05"
  } else if (driver == "pop") {
    unit <- "Mio people"
  } else if (driver == "urban") {
    unit <- "Mio people"
  } else {
    unit <- "population: Mio people, gdp: Million USD, urban population: Mio people"
  }

  return(list(x = combined,
              weight = NULL,
              unit = unit,
              description = "collects all data necessary to create demand projections"
  ))
}
