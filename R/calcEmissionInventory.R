#' @title calcEmisNitrogenCroplandPast
#' @description providees an emission inventory for the past, either from external data or own estimates.
#'
#' @param datasource The Inventory that shall be used. Options are CEDS, combined_CEDS_IPCC
#' (including own estimates where available), IPCC(own estimates), Nsurplus (own estimates)
#' @param targetResolution Specific mapping file to be used.
#' @param from column in mapping
#' @param to column in mapping
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky, Michael S. Crawford
#' @examples
#' \dontrun{
#' calcOutput("EmisNitrogenCroplandPast")
#' }
#'
calcEmissionInventory <- function(datasource = "CEDS", targetResolution = "sectoral", from = "CEDS59", to = "Sectors") {

  past <- findset("past")
  if (datasource == "CEDS") {

    # read CEDS emissions data from sources
    bc    <- readSource("CEDS", subtype = "BC")
    # ch4   <- readSource("CEDS",subtype="CH4") #excluding ghg emissions, they were strange # nolint
    co    <- readSource("CEDS", subtype = "CO")
    # co2   <- readSource("CEDS",subtype="CO2")#excluding ghg emissions, they were strange # nolint
    # n2o   <- readSource("CEDS",subtype="N2O") #excluding ghg emissions, they were strange # nolint
    nh3   <- readSource("CEDS", subtype = "NH3")
    nox   <- readSource("CEDS", subtype = "NOx")
    nmvoc <- readSource("CEDS", subtype = "NMVOC")
    oc    <- readSource("CEDS", subtype = "OC")
    so2   <- readSource("CEDS", subtype = "SO2")


    # identify common years (starting in 1970 at least ch4 has less years that the other gases)
    y <- Reduce(intersect, list(getYears(bc),
                               #  getYears(ch4), # nolint
                               getYears(co),
                               #   getYears(co2), # nolint
                               #  getYears(n2o), # nolint
                               getYears(nh3),
                               getYears(nox),
                               getYears(nmvoc),
                               getYears(oc),
                               getYears(so2)))

    ceds <- mbind(bc[, y, ],
                  # ch4[,y,], # nolint
                  co[, y, ],
                  # co2[,y,], # nolint
                  # n2o[,y,], # nolint
                  nh3[, y, ],
                  nox[, y, ],
                  nmvoc[, y, ],
                  oc[, y, ],
                  so2[, y, ]) / 1000 # kt -> Mt # nolint
    rm(bc,
       # ch4,
       co,
       # co2,
       # n2o,
       nh3,
       nox,
       nmvoc,
       oc,
       so2)

    # "past" may contain more years than ceds -> add missing years
    missingY <- setdiff(past, y)
    ceds <- add_columns(ceds, missingY, dim = 2.1)

    # rename emissions according to map
    map2 <- c(BC = "bc",
              # CH4="ch4", # nolint
              CO = "co",
              # CO2="co2_c", # nolint
              # N2O="n2o_n", # nolint
              NH3 = "nh3_n",
              NOx = "no2_n",
              NMVOC = "nmhc",
              OC = "oc",
              SO2 = "so2")
    getNames(ceds, dim = 2) <- map2[getNames(ceds, dim = 2)]

    # remove third entry "kt" in data dimension
    ceds <- collapseNames(ceds, collapsedim = 3)

    # add nitrate leaching with zeros (to keeep a unique format with other sources)
    ceds <- add_columns(ceds[, past, ], addnm = "no3_n", dim = 3.2)
    ceds[, , "no3_n"] <- 0
    # ceds[,,"n2o_n"]<-0 # this data seems buggy # nolint


    # correct units
    # ceds[,,"n2o_n"]=ceds[,,"n2o_n"]/44*28 # nolint
    ceds[, , "nh3_n"] <- ceds[, , "nh3_n"] / 17 * 14
    ceds[, , "no2_n"] <- ceds[, , "no2_n"] / 46 * 14
    # ceds[,,"co2_c"]=ceds[,,"co2_c"]/44*12 # nolint
    out <- ceds

    } else if (datasource == "EDGAR6") {

    edgarEmissionNames <- list("n2o",
                               "ch4",
                               "co2_excl_short", "co2_incl_short",
                               "nh3",
                               "no2",
                               "bc",
                               "co",
                               "oc",
                               # "nmvoc",
                               # "pm10",
                               # "pm25",
                               "so2")

    edgarList <- lapply(X = edgarEmissionNames, FUN = function(x) readSource("EDGAR6", subtype = x))
    names(edgarList) <- edgarEmissionNames

    # Find years common to all datasets
    yearsPresent <- Reduce(f = intersect, x = Map(getYears, edgarList))

    edgarList <- lapply(X = edgarList,
                        FUN = function(x) {

                          x <- x[, yearsPresent, ] # Eliminate incomplete temporal ranges between emissions

                          # Summation over dim 3.3, bio vs. fossil when appropriate
                          if (dimExists(3.3, x)) {
                            x <- dimSums(x, dim = 3.3)
                          }

                          x <- x / 1000 # kt to Mt

                          return(x)
                        })

    # Consolidate "co2_excl_short" and "co2_incl_short" into one MAgPIE object
    edgarCo2 <- mbind(edgarList[c("co2_excl_short", "co2_incl_short")])
    edgarCo2 <- dimSums(edgarCo2, dim = 3.1)
    edgarCo2 <- add_dimension(edgarCo2, dim = 3.1, nm = "co2")
    edgarList <- edgarList[!names(edgarList) %in% c("co2_excl_short", "co2_incl_short")]
    edgarList <- c(edgarList, list(co2 = edgarCo2))

     # Unit conversion when appropriate (e.g. n2o to n2o_n)
    .formatToMAgPIE <- function(x, new, conversion) {
      getNames(x, dim = 1) <- new
      x[, , new] <- x[, , new] * conversion

      return(x)
    }

    edgarList[["n2o"]] <- .formatToMAgPIE(edgarList[["n2o"]], "n2o_n", (28 / 44))
    edgarList[["nh3"]] <- .formatToMAgPIE(edgarList[["nh3"]], "nh3_n", (14 / 17))
    edgarList[["no2"]] <- .formatToMAgPIE(edgarList[["no2"]], "no2_n", (14 / 46))
    edgarList[["co2"]] <- .formatToMAgPIE(edgarList[["co2"]], "co2_c", (12 / 44))

    edgar <- mbind(edgarList)

    # EDGAR has incomplete temporal coverage for many countries
    edgar <- toolConditionalReplace(edgar, "is.na()", 0)

    # Remove names wherein all the underlying data are 0
    toKeep <- lapply(X = getNames(edgar), FUN = function(x) !(all(edgar[, , x] == 0)))
    edgar <- edgar[, , unlist(toKeep)]

    # Filter down to the sectors relevant to MAgPIE
    relevantSectors <- c("3_C_1 Emissions from biomass burning",
                         "3_A_1 Enteric Fermentation",
                         "3_A_2 Manure Management",
                         "3_C_7 Rice cultivations",
                         "3_C_4 Direct N2O Emissions from managed soils",
                         "3_C_5 Indirect N2O Emissions from managed soils")

    edgar <- edgar[, , relevantSectors]

    # Rename dimensions
    names(dimnames(edgar)) <- c("Region", "Year", "pollutant.sector")

    out <- edgar

    } else if (datasource == "CEDS2021") {

    # read CEDS emissions data from sources
    ceds    <- readSource("CEDS2021")

    ceds <- ceds[, paste0("y", 1960:2019), ]

    # add nitrate leaching with zeros (to keeep a unique format with other sources)
    ceds <- add_columns(ceds[, , ], addnm = "no3_n", dim = 3.2)
    ceds[, , "no3_n"] <- 0

    out <- ceds
  } else if (datasource %in% c("combined_CEDS_IPCC")) {

    ceds <- calcOutput("EmissionInventory",
                     datasource = "CEDS",
                     targetResolution = NULL,
                     aggregate = FALSE)

    ipcc <- calcOutput("EmissionInventory",
                     datasource = "IPCC",
                     targetResolution = NULL,
                     aggregate = FALSE)

    # replace<-c("3B_Manure-management","3D_Soil-emissions") # nolint
    # "3E_Enteric-fermentation","3F_Agricultural-residue-burning-on-fields","3D_Rice-Cultivation"
    # ag_emis_in_magpie<-c("4B","4D1","4D2") # nolint
    # ag_emis_in_magpie<-c("4B","4D1","4D2") # nolint
    # ag_emis_in_magpie<-c("4D1") # nolint
    ipcc <- add_columns(ipcc, addnm = "n2o_n", dim = 3.2)
    ipcc[, , "n2o_n"] <- dimSums(ipcc[, , c("n2o_n_direct", "n2o_n_indirect")])

    jointEmissions <- intersect(getNames(ipcc, dim = 2), getNames(ceds, dim = 2))
    ceds[, , "3B_Manure-management"][, , jointEmissions] <- ipcc[, , "awms"][, , jointEmissions]
    ceds[, , "3D_Soil-emissions"][, , jointEmissions] <- dimSums(ipcc[, , c("inorg_fert", "man_crop", "resid", "som", "rice", "pasture_soils")][, , jointEmissions], dim = 3.1) # nolint
    out <- ceds
  } else if (datasource %in% c("combined_CEDS_Nsurplus2")) {

    ceds <- calcOutput("EmissionInventory",
                     datasource = "CEDS",
                     targetResolution = NULL,
                     aggregate = FALSE)

    ipcc <- calcOutput("EmissionInventory",
                     datasource = "Nsurplus2",
                     targetResolution = NULL,
                     aggregate = FALSE)

    # replace<-c("3B_Manure-management","3D_Soil-emissions") # nolint
    # "3E_Enteric-fermentation","3F_Agricultural-residue-burning-on-fields","3D_Rice-Cultivation"
    # ag_emis_in_magpie<-c("4B","4D1","4D2") # nolint
    # ag_emis_in_magpie<-c("4B","4D1","4D2") # nolint
    # ag_emis_in_magpie<-c("4D1") # nolint

    ipcc <- add_columns(ipcc, addnm = "n2o_n", dim = 3.2)
    ipcc[, , "n2o_n"] <- dimSums(ipcc[, , c("n2o_n_direct", "n2o_n_indirect")])

    jointEmissions <- intersect(getNames(ipcc, dim = 2), getNames(ceds, dim = 2))
    ceds[, , "3B_Manure-management"][, , jointEmissions] <- collapseNames(ipcc[, , "awms"][, , jointEmissions])
    ceds[, , "3D_Soil-emissions"][, , jointEmissions] <- dimSums(ipcc[, , c("cropland_soils", "pasture_soils")][, , jointEmissions], dim = 3.1) # nolint

    out <- ceds

  } else if (datasource %in% c("IPCC", "Nsurplus", "Nsurplus2")) {
    out <- calcOutput("EmisNitrogenPast", method = datasource, aggregate = FALSE)

  } else if (datasource %in% c("combined_CEDS_PRIMAPhist")) {

    if (to != "PRIMAPhist") {
      stop("This datasource is only available with the PRIMAPhist sectoral mapping.
      Please set argument to = 'PRIMAPhist'")
    }

    mapping <- NULL
    ceds <- calcOutput("EmissionInventory",
                     datasource = "CEDS",
                     targetResolution = NULL,
                     aggregate = FALSE)

    primap <- readSource("PRIMAPhist", subtype = "hist")


    # find years overlap
    jointYears <- intersect(getYears(ceds), getYears(primap))

    # aggregate ceds categories to primap categories
    map <- toolGetMapping(type = "sectoral", name = "mappingCEDS59toPRIMAP.csv")

    cedsAgg <- toolAggregate(ceds, map, dim = 3.1)
    getSets(primap) <- getSets(ceds)
    getSets(cedsAgg) <- getSets(ceds)

    jointEmissions <- intersect(getNames(primap, dim = 2), getNames(cedsAgg, dim = 2))


    # use PRIMAP data for joint emissions
    cedsEmissions <- getNames(cedsAgg, dim = 2)[!(getNames(cedsAgg, dim = 2) %in% jointEmissions)]
    cedsAgg <- cedsAgg[, , cedsEmissions]

    out <- mbind(cedsAgg[, jointYears, ], primap[, jointYears, ])


  } else {
stop("datasource unknown")
}

  if (!is.null(targetResolution)) {

    # aggregate and rename CEDS59 sectors to CEDS16 sectors
    if (targetResolution == "sectoral") {
      map <- toolGetMapping(type = "sectoral", name = "mappingCEDS59toSectors.csv")
    } else if (targetResolution == "magpie") {
      map <- toolGetMapping(type = "sectoral", name = "mappingCEDS59toMAgPIE.csv")
    } else {
      stop("Unknown target resolution \"", targetResolution, "\".")
    }

    # reduce ceds to available categories
    out <- out[, , getNames(out, dim = 1)[getNames(out, dim = 1) %in% map[, which(names(map) == from)]]]

    out <- groupAggregate(data = out, dim = 3.1, query = map, from = from, to = to)

  }

  return(list(x = out,
              weight = NULL,
              unit = "Mt",
              description = "Emission inventories from various datasources. Nitrogen emissions are in N equivalents,
              CO2 in C equivalents, all other in their original molecular weight.")
  )
}
