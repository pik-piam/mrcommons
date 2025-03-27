#' Convert IEA
#'
#' Convert IEA energy data to data on ISO country level.
#'
#' @param x MAgPIE object containing IEA values at IEA mixed country-region
#' resolution
#' @param subtype data subtype. Either "EnergyBalances", "EnergyBalances-latest",
#' or "Emissions"
#' @return IEA data as MAgPIE object aggregated to country level
#' @author Anastasis Giannousakis, Renato Rodrigues, Falk Benke
#' @importFrom dplyr %>% filter
#' @importFrom tidyr unite
#'
convertIEA <- function(x, subtype) {
  if (grepl("EnergyBalances", subtype)) {
    # aggregate Kosovo to Serbia
    x1 <- x["KOS", , ]
    getItems(x1, dim = 1) <- c("SRB")
    x["SRB", , ] <- x["SRB", , ] + x1
    x <- x[c("KOS"), , , invert = TRUE]

    # convert electricity outputs (unit conversion between ktoe and GWh)
    x[, , c("ELOUTPUT", "ELMAINE", "ELAUTOE", "ELMAINC", "ELAUTOC")] <- 0.0859845 *
      x[, , c("ELOUTPUT", "ELMAINE", "ELAUTOE", "ELMAINC", "ELAUTOC")]

    # calculate weight to be used for regional disaggregations
    wp <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[, 2010, ]
    wg <- calcOutput("GDP", scenario = "SSP2", aggregate = FALSE)[, 2010, ]
    wp <- wp / max(wp)
    wg <- wg / max(wg)
    w <- wp + wg

    # disaggregating Other Africa (IAF),
    # Other non-OECD Americas (ILA) and
    # Other non-OECD Asia (IAS) regions to countries
    mappingfile <- toolGetMapping(
      type = "regional", name = "regionmappingIeaOther2016.csv",
      returnPathOnly = TRUE, where = "mrcommons"
    )
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
    x <- toolCountryFill(x, 0, verbosity = 2)

    # These changes may reduce the amount of CHP plants to below what is actually
    # deployed in a region, because heat reporting is obscure. In some statistics,
    # heat from CHP plant used in the same industrial compound is NOT explicitly
    # reported as heat. If this is the case in the IEA data as well, this would
    # lead to underestimating CHP plants. However, there currently seems to be no
    # better way to create consistent data for CHP/non-CHP electricity production
    # and heat ouput.

    # for each product: check if the flow "HEMAINC" > 0, if yes, do nothing;
    # if no, add the value of the flow "ELMAINC" to "ELMAINE" and afterwards set ELMAINC to zero.

    missingFlows <- setdiff(
      expand.grid(
        iea_product = getItems(x[, , "ELMAINE"], dim = 3.1) %>% # nolint
          union(getItems(x[, , "ELMAINC"], dim = 3.1)) %>%
          union(getItems(x[, , "HEMAINC"], dim = 3.1)), iea_flows = c("ELMAINE", "ELMAINC", "HEMAINC")
      ) %>%
        unite("product.flow", c("iea_product", "iea_flows"), sep = ".") %>%
        pull("product.flow"), getItems(x, dim = 3)
    )

    x <- add_columns(x, addnm = missingFlows, dim = 3, fill = 0)

    d <- x[, , c("ELMAINE", "ELMAINC", "HEMAINC")]
    tmp <- mcalc(d, ELMAINE ~ ifelse(HEMAINC > 0, ELMAINE, ELMAINC + ELMAINE), append = FALSE)
    x[, , "ELMAINE"] <- tmp
    tmp <- mcalc(d, ELMAINC ~ ifelse(HEMAINC > 0, ELMAINC, 0), append = FALSE)
    x[, , "ELMAINC"] <- tmp

    # for each product: check if the flow "HEAUTOC" > 0, if yes, do nothing;
    # if no, add the value of the flow "ELAUTOC" to "ELAUTOE" and afterwards set ELAUTOC to zero.

    missingFlows <- setdiff(
      expand.grid(
        iea_product = getItems(x[, , "ELAUTOE"], dim = 3.1) %>% # nolint
          union(getItems(x[, , "ELAUTOC"], dim = 3.1)) %>%
          union(getItems(x[, , "HEAUTOC"], dim = 3.1)), iea_flows = c("ELAUTOE", "HEAUTOC", "ELAUTOC")
      ) %>%
        unite("product.flow", c("iea_product", "iea_flows"), sep = ".") %>%
        pull("product.flow"), getItems(x, dim = 3)
    )

    x <- add_columns(x, addnm = missingFlows, dim = 3, fill = 0)

    d <- x[, , c("ELAUTOE", "HEAUTOC", "ELAUTOC")]
    tmp <- mcalc(d, ELAUTOE ~ ifelse(HEAUTOC > 0, ELAUTOE, ELAUTOC + ELAUTOE), append = FALSE)
    x[, , "ELAUTOE"] <- tmp
    tmp <- mcalc(d, ELAUTOC ~ ifelse(HEAUTOC > 0, ELAUTOC, 0), append = FALSE)
    x[, , "ELAUTOC"] <- tmp


    # Correct transport reporting issue in IEA data for NONBIODIES.MARBUNK in RUS
    # FE is reported in 1990 and 2010 but not in the years in between.
    # This cause problems in the harmonization of EDGE-Transport and the IEA data
    # in 2005 as there is no MARBUNK demand at all for REF regions.

    x["RUS", seq(1990, 2010, 1), "NONBIODIES.MARBUNK"] <-
      x["RUS", c(1990, 2010), "NONBIODIES.MARBUNK"] |> time_interpolate(seq(1990, 2010, 1))

    # Adjust totals
    x["RUS", seq(1991, 2009, 1), "TOTAL.MARBUNK"] <-
      x["RUS", seq(1991, 2009, 1), "TOTAL.MARBUNK"] + x["RUS", seq(1991, 2009, 1), "NONBIODIES.MARBUNK"]
  }

  return(x)
}
