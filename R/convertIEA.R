#' Convert IEA
#' 
#' Convert IEA energy data to data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing IEA values at IEA mixed country-region
#' resolution
#' @param subtype data subtype. Either "EnergyBalances" or "CHPreport"
#' @return IEA data as MAgPIE object aggregated to country level
#' @author Anastasis Giannousakis, Renato Rodrigues, Falk Benke
 
convertIEA <- function(x,subtype) {

  if (subtype == "EnergyBalances") {

    # aggregate Kosovo to Serbia
    x1 <- x["KOS", , ]
    getItems(x1, dim = 1) <- c("SRB")
    x["SRB", , ] <- x["SRB", , ] + x1
    x <- x[c("KOS"), , , invert = TRUE]
    
    # convert electricity outputs (unit conversion between ktoe and GWh)
    x[, , c("ELOUTPUT", "ELMAINE", "ELAUTOE", "ELMAINC", "ELAUTOC")] <- x[, , c("ELOUTPUT", "ELMAINE", "ELAUTOE", "ELMAINC", "ELAUTOC")] * 0.0859845

    # calculate weight to be used for regional disaggregations
    wp <- calcOutput("Population", aggregate = F, FiveYearSteps = F)[, 2010, "pop_SSP2"]
    wg <- calcOutput("GDP", aggregate = FALSE, FiveYearSteps = FALSE)[, 2010, "gdp_SSP2"]
    wp <- wp / max(wp)
    getNames(wp) <- "SSP2"
    wg <- wg / max(wg)
    getNames(wg) <- "SSP2"
    w <- wp + wg
    
    # disaggregating Other Africa (IAF), Other non-OECD Americas (ILA) and Other non-OECD Asia (IAS) regions to countries
    mappingfile <- toolGetMapping(type = "regional", name = "regionmappingIEA.csv", returnPathOnly = TRUE)
    mapping <- read.csv2(mappingfile, stringsAsFactors = TRUE)
    xadd <- toolAggregate(x[levels(mapping[[2]]), , ], mappingfile, weight = w[as.vector(mapping[[1]]), , ])
    x <- x[setdiff(getItems(x, dim = 1), as.vector(unique(mapping[[2]]))), , ]
    x <- mbind(x, xadd)

    # disaggregating extinct countries USSR (SUN) and Yugoslavia (YUG)
    ISOhistorical <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"), stringsAsFactors = F)
    ISOhistorical <- ISOhistorical[!ISOhistorical$toISO == "SCG", ]
    x <- toolISOhistorical(x, mapping = ISOhistorical,
      additional_weight = w[ISOhistorical[ISOhistorical$fromISO %in% c("YUG", "SUN"), "toISO"], , ]
    )
    x[is.na(x)] <- 0

    # filling missing country data
    x <- toolCountryFill(x, 0)

  } else if (subtype == "CHPreport") {
    # adjust the share for some region to avoid infeasibilities in the initial year
    x["RUS",,] <- 70
    x[c("BGR","CZE","POL","ROU","SVK"),,] <- 70
    x[c("BEL","LUX","NLD"),,] <- 40
  }
  
  
  return(x)
}  
