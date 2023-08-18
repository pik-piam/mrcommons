#' Convert IEA
#'
#' Convert IEA energy data to data on ISO country level.
#'
#'
#' @param x MAgPIE object containing IEA values at IEA mixed country-region
#' resolution
#' @param subtype data subtype. Either "EnergyBalances" or "Emissions"
#' @return IEA data as MAgPIE object aggregated to country level
#' @author Anastasis Giannousakis, Renato Rodrigues, Falk Benke
#' @importFrom dplyr %>% filter
#' @importFrom tidyr unite
#'
convertIEA <- function(x, subtype) {
  if (subtype == "EnergyBalances") {

    # aggregate Kosovo to Serbia
    x1 <- x["KOS", , ]
    getItems(x1, dim = 1) <- c("SRB")
    x["SRB", , ] <- x["SRB", , ] + x1
    x <- x[c("KOS"), , , invert = TRUE]

    # convert electricity outputs (unit conversion between ktoe and GWh)
    x[, , c("ELOUTPUT", "ELMAINE", "ELAUTOE", "ELMAINC", "ELAUTOC")] <- 0.0859845 *
                                                        x[, , c("ELOUTPUT", "ELMAINE", "ELAUTOE", "ELMAINC", "ELAUTOC")]

    # calculate weight to be used for regional disaggregations
    wp <- calcOutput("Population", aggregate = FALSE)[, 2010, "pop_SSP2"]
    wg <- calcOutput("GDP", aggregate = FALSE)[, 2010, "gdp_SSP2"]
    wp <- wp / max(wp)
    getNames(wp) <- "SSP2"
    wg <- wg / max(wg)
    getNames(wg) <- "SSP2"
    w <- wp + wg

    # disaggregating Other Africa (IAF),
    # Other non-OECD Americas (ILA) and
    # Other non-OECD Asia (IAS) regions to countries
    mappingfile <- toolGetMapping(type = "regional", name = "regionmappingIEA_Other2016.csv",
                                  returnPathOnly = TRUE, where = "mappingfolder")
    mapping <- read.csv2(mappingfile, stringsAsFactors = TRUE) %>%
      filter(!(!!sym("CountryCode") %in% getItems(x, dim = 1)))
    xadd <- toolAggregate(x[levels(mapping[[2]]), , ], mapping, weight = w[as.vector(mapping[[1]]), , ])
    x <- x[setdiff(getItems(x, dim = 1), as.vector(unique(mapping[[2]]))), , ]
    x <- mbind(x, xadd)

    # disaggregating extinct countries USSR (SUN) and Yugoslavia (YUG)
    ISOhistorical <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"), stringsAsFactors = FALSE) # nolint
    ISOhistorical <- ISOhistorical[!ISOhistorical$toISO == "SCG", ] # nolint
    x <- toolISOhistorical(x,
      mapping = ISOhistorical,
      additional_weight = w[ISOhistorical[ISOhistorical$fromISO %in% c("YUG", "SUN"), "toISO"], , ]
    )
    x[is.na(x)] <- 0

    # filling missing country data
    x <- toolCountryFill(x, 0)

    # These changes may reduce the amount of CHP plants to below what is actually
    # deployed in a region, because heat reporting is obscure. In some statistics,
    # heat from CHP plant used in the same industrial compound is NOT explicitly
    # reported as heat. If this is the case in the IEA data as well, this would
    # lead to underestimating CHP plants. However, there currently seems to be no
    # better way to create consistent data for CHP/non-CHP electricity production
    # and heat ouput.

    # for each product: check if the flow "HEMAINC" > 0, if yes, do nothing;
    # if no, add the value of the flow "ELMAINC" to "ELMAINE" and afterwards set ELMAINC to zero.

    missing.flows <- setdiff(expand.grid(iea_product = getItems(x[, , "ELMAINE"], dim = 3.1) %>% # nolint
      union(getItems(x[, , "ELMAINC"], dim = 3.1)) %>%
      union(getItems(x[, , "HEMAINC"], dim = 3.1)), iea_flows = c("ELMAINE", "ELMAINC", "HEMAINC")) %>%
      unite("product.flow", c("iea_product", "iea_flows"), sep = ".") %>%
      pull("product.flow"), getItems(x, dim = 3))

    x <- add_columns(x, addnm = missing.flows, dim = 3, fill = 0)

    d <- x[, , c("ELMAINE", "ELMAINC", "HEMAINC")]
    tmp <- mcalc(d, ELMAINE ~ ifelse(HEMAINC > 0, ELMAINE, ELMAINC + ELMAINE), append = FALSE)
    x[, , "ELMAINE"] <- tmp
    tmp <- mcalc(d, ELMAINC ~ ifelse(HEMAINC > 0, ELMAINC, 0), append = FALSE)
    x[, , "ELMAINC"] <- tmp

    # for each product: check if the flow "HEAUTOC" > 0, if yes, do nothing;
    # if no, add the value of the flow "ELAUTOC" to "ELAUTOE" and afterwards set ELAUTOC to zero.

    missing.flows <- setdiff(expand.grid(iea_product = getItems(x[, , "ELAUTOE"], dim = 3.1) %>% # nolint
      union(getItems(x[, , "ELAUTOC"], dim = 3.1)) %>%
      union(getItems(x[, , "HEAUTOC"], dim = 3.1)), iea_flows = c("ELAUTOE", "HEAUTOC", "ELAUTOC")) %>%
      unite("product.flow", c("iea_product", "iea_flows"), sep = ".") %>%
      pull("product.flow"), getItems(x, dim = 3))

    x <- add_columns(x, addnm = missing.flows, dim = 3, fill = 0)

    d <- x[, , c("ELAUTOE", "HEAUTOC", "ELAUTOC")]
    tmp <- mcalc(d, ELAUTOE ~ ifelse(HEAUTOC > 0, ELAUTOE, ELAUTOC + ELAUTOE), append = FALSE)
    x[, , "ELAUTOE"] <- tmp
    tmp <- mcalc(d, ELAUTOC ~ ifelse(HEAUTOC > 0, ELAUTOC, 0), append = FALSE)
    x[, , "ELAUTOC"] <- tmp
  }

  return(x)
}
