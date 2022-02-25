#' @title toolCalcIEAfromStructureMappingPEFE
#' @description mapping IEA reported data to ReMIND-MAgPIE items
#'
#' @param data data to map
#' @param structureMapping mapping to use
#' @param subtype remind (default), edge, pfu or magpie
#'
#' @return MAgPIE object with completed time dimensionality.
#' @author Anastasis Giannousakis, Lavinia Baumstark, Isabelle Weindl
#'
#' @importFrom dplyr %>% filter
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @export
toolCalcIEAfromStructureMappingPEFE <- function(data, structureMapping, subtype = "remind") {
  # choose the name of the column which should be targeted in the structureMapping
  if  (subtype == "remind") {
    targetName <- "REMINDitems_out"
  } else if (subtype == "edge") {
    targetName <- "EDGEitems"
  } else if (subtype == "pfu") {
    targetName <- "pfu"
  } else if (subtype == "magpie") {
    targetName <- "magpie_items"
  } else stop("valid subtypes are 'remind', 'edge', 'pfu' and 'magpie'")

rawMapping <- read.csv2(structureMapping, stringsAsFactors = FALSE)
ieamatch <- na.omit(rawMapping[c("iea_product", "iea_flows", targetName, "Weight")])

# take only the items that are asigned to model categories
ieamatch <- subset(ieamatch, !grepl("not_used", ieamatch[[targetName]]))

regions <- getItems(data, dim = 1.1)
years <- getYears(data)

# take only the items at the intersection of data and ieamatch
flowsIntersect <- intersect(getNames(data, dim = "FLOW"), unique(ieamatch[["iea_flows"]]))
prodIntersect <- intersect(getNames(data, dim = "PRODUCT"), unique(ieamatch[["iea_product"]]))

ieamatch <- ieamatch %>% filter(.data$iea_flows %in% flowsIntersect, .data$iea_product %in% prodIntersect)

# create an empty mapgie object that will be filled with aggregated items
outputnames <- paste(rep(unique(ieamatch[[targetName]]), each = length(flowsIntersect)), flowsIntersect, sep = ".")
targetitems <- as.magpie(array(dim = c(length(regions), length(years), length(outputnames)),
                               dimnames = list(regions, years, outputnames)))


for (item in getNames(targetitems, dim = 1)) {
  map <- ieamatch[ieamatch[[targetName]] == item, c("iea_product", "iea_flows")]
  mapSub <- paste(map[["iea_product"]], map[["iea_flows"]], sep = ".")
  data[, , mapSub] <- data[, , mapSub] * as.numeric(ieamatch[rownames(map), "Weight"])
  targetitems[, , item] <- dimSums(data[, , mapSub], dim = 3.1, na.rm = TRUE)
}

return(targetitems)
}
