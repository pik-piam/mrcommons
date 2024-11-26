#' calcIOEdgeBuildings
#'
#' Calculates buildings-related energy flows from the IEA energy balances.
#' 'output_EDGE_buildings' is a key input to EDGE-Buildings providing the
#' historic final energy demand from buildings. 'output_EDGE' does the same for
#' buildings and industry together.
#'
#' @param subtype Data subtype. See default argument for possible values.
#' @param ieaVersion Release version of IEA data, either 'default'
#' (vetted and used in REMIND) or 'latest'.
#' @returns IEA data as MAgPIE object aggregated to country level
#'
#' @author Pascal Sauer, Anastasis Giannousakis, Robin Hasse
#'
#' @examples
#' \dontrun{
#' a <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings")
#' }
#'
#' @seealso \code{\link{calcOutput}}
#'
#' @importFrom dplyr %>% .data all_of filter select
#' @importFrom tidyr unite
#' @importFrom madrat readSource toolGetMapping toolAggregate calcOutput
#' @importFrom utils read.csv2
#' @importFrom magclass as.magpie getNames mselect

calcIOEdgeBuildings <- function(subtype = c("output_EDGE", "output_EDGE_buildings"),
                                ieaVersion = c("default", "latest")) {

  subtype <- match.arg(subtype)
  ieaVersion <- match.arg(ieaVersion)



  # READ -----------------------------------------------------------------------

  # convert from ktoe to EJ
  data <- switch(ieaVersion,
    default = readSource("IEA", subtype = "EnergyBalances"),
    latest  = readSource("IEA", subtype = "EnergyBalances-latest")
  ) * 4.1868e-5



  # AGGREGATE ------------------------------------------------------------------

  target <- switch(subtype,
    output_EDGE = "EDGEitems",
    output_EDGE_buildings = "EDGE_buildings"
  )

  mapping <- toolGetMapping(type = "sectoral",
                            name = "structuremappingIO_outputs.csv",
                            where = "mrcommons", returnPathOnly = TRUE) %>%
    read.csv2(stringsAsFactors = FALSE, na.strings = "") %>%
    select(all_of(c("iea_product", "iea_flows", target, "Weight"))) %>%
    na.omit() %>%
    unite("target", all_of(target), sep = ".") %>%
    unite("product.flow", c("iea_product", "iea_flows"), sep = ".", remove = FALSE) %>%
    filter(.data[["product.flow"]] %in% getNames(data),
           .data[["Weight"]] != 0) %>%
    mutate(Weight = as.numeric(.data[["Weight"]]))

  weight <- as.magpie(mapping[, c("iea_product", "iea_flows", "Weight")])

  data <- toolAggregate(data[, , mapping[["product.flow"]]] * weight,
                        rel = mapping, from = "product.flow", to = "target", dim = 3)
  getSets(data)[3] <- "d3"



  # SPLIT BIOMASS --------------------------------------------------------------

  gdppop <- calcOutput("GDPpc", average2020 = FALSE, aggregate = FALSE) %>%
    mselect(variable = "gdppc_SSP2", collapseNames = TRUE)

  data <- switch(subtype,
    output_EDGE = toolSplitBiomass(data, gdppop, split = "feresbioshare",
                                   into = c("feresbiotrad", "feresbiomod")),
    output_EDGE_buildings = toolSplitBiomass(data, gdppop, split = "bioshare")
  )

  return(list(x = data,
              weight = NULL,
              unit = "EJ/yr",
              description = paste("Historic FE demand from buildings",
                                  "(and industry) based on IEA Energy Balances")))
}
