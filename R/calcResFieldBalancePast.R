#' @title calcResFieldBalancePast
#' @description Calculates data for aboveground and belowground residues production with other usage
#'
#' @param cellular If TRUE calculation and output on cellular level
#' @param products "sum" (default) or "kres"
#'
#' @return data
#' @author Benjamin Bodirsky
#' @seealso [madrat::calcOutput()], [madrat::readSource()]
#' @examples
#' \dontrun{
#' calcOutput("ResFieldBalancePast")
#' }
#'
#' @importFrom magpiesets findset

calcResFieldBalancePast <- function(cellular = FALSE, products = "sum") {

  if (products == "kres") {
    relevantNutrients <- c("nr", "p", "k", "c")  # after burning, unclear what dm and ge may be

    production        <- collapseNames(calcOutput("ResBiomass", cellular = cellular, plantparts = "ag",
                                                  aggregate = FALSE))[, , relevantNutrients]

    burnshr           <- calcOutput("ResCombustEff", aggregate = FALSE)[, , getNames(production, dim = 1)]
    devStatePast      <- collapseNames(calcOutput("DevelopmentState", aggregate = FALSE)[, , "SSP2"])
    commonYears  <- intersect(getYears(production), getYears(devStatePast))
    devStatePast <- devStatePast[, commonYears, ]
    production   <- production [, commonYears, ]

    if (cellular) {
      devStatePast    <- toolIso2CellCountries(devStatePast)
    }

    # if the following parameters are changed, they also have to be changed in the GAMS code!
    # incorporate a phase-out of agricultural residue burning that begins in the year 1995
    historicalYears <- subset(findset("time"), findset("time") %in% getItems(devStatePast)$year)

    developedBurnRatio   <- new.magpie(years = findset("time"), fill = 0.15)
    developedPhaseout    <- convergence(origin = developedBurnRatio, start_year = "y1995", aim = 0,
                                        end_year = "y2025", type = "linear")
    developedPhaseout    <- developedPhaseout[, historicalYears, ]

    developingBurnRatio  <- new.magpie(years = findset("time"), fill = 0.25)
    developingPhaseout   <- convergence(origin = developingBurnRatio, start_year = "y1995", aim = 0.1,
                                        end_year = "y2025", type = "linear")
    developingPhaseout   <- developingPhaseout[, historicalYears, ]

    developedHistoricalBurning  <- devStatePast * developedPhaseout
    developingHistoricalBurning <- (1 - devStatePast) * developingPhaseout
    burn <- ash                 <- production * (developedHistoricalBurning + developingHistoricalBurning)

    # assuming the same for C and Nr, maybe has to be updated
    ash[, , c("c", "nr")] <- ash[, , c("c", "nr")] * (1 - burnshr)
    burn               <- burn - ash

    mapping       <- toolGetMapping("mappingCrop2Residue.csv", where = "mrcommons", type = "sectoral")
    burn          <- toolAggregate(burn,       rel = mapping, from = "kcr", to = "kres", dim = 3.1)
    ash           <- toolAggregate(ash,        rel = mapping, from = "kcr", to = "kres", dim = 3.1)
    production    <- toolAggregate(production, rel = mapping, from = "kcr", to = "kres", dim = 3.1)

    ### estimate removal

    if (cellular) {
      # to avoid negative values, take the regional share of removal by product
      fieldbalance <- calcOutput("ResFieldBalancePast", cellular = FALSE, aggregate = FALSE, products = "kres")
      # use nr for removalshare decision
      removalshare <- collapseNames((fieldbalance[, , "removal"]
                                     / (fieldbalance[, , "biomass"]
                                        - fieldbalance[, , "burned"] - fieldbalance[, , "ash"]))[, , "nr"])
      removalshare[is.nan(removalshare)] <- 1

      removalshare <- toolIso2CellCountries(removalshare)
      cell2Coord   <- toolGetMappingCoord2Country(pretty = TRUE)
      removalshare <- toolAggregate(x = removalshare, rel = cell2Coord,
                                    from = "iso", to = "coords", partrel = TRUE)
      cyears <- intersect(getYears(production), getYears(removalshare))
      removalshare <- removalshare[, cyears, ]
      removal <- (production - burn - ash) * removalshare

    } else {
      removal      <- collapseNames(calcOutput("ResDemand", cellular = FALSE,
                                               aggregate = FALSE)[, , "domestic_supply"])[, , relevantNutrients]
      removal      <- add_columns(removal, addnm = c("res_nouse"), dim = 3.1)
      removal[, , c("res_nouse")] <- 0
    }

    commonYears <- intersect(getYears(removal), getYears(production))
    removal     <- removal[, commonYears, ]
    production  <- production [, commonYears, ]
    burn        <- burn[, commonYears, ]
    ash         <- ash [, commonYears, ]
    recycle <- production - removal - burn

    ### check for negative recycling shares and decrease removal if nesseccary
    correctRemoval <- recycle
    correctRemoval[correctRemoval > 0] <- 0

    removal <- removal + correctRemoval
    recycle <- round(production - removal - burn, 8)  # taking ash into recycling
    removal <- round(removal, 8)

    if (any(correctRemoval != 0)) {
      vcat(2, "Residue removal was corrected in areas, where there was not enough residue biomass available.")
    }

    ### generate output
    out <- mbind(add_dimension(production, dim = 3.1, nm = "biomass"),
                 add_dimension(removal, dim = 3.1, nm = "removal"),
                 add_dimension(burn, dim = 3.1, nm = "burned"),
                 add_dimension(ash, dim = 3.1, nm = "ash"),
                 add_dimension(recycle, dim = 3.1, nm = "recycle"))

  } else if (products == "sum") {

    out <- calcOutput("ResFieldBalancePast", cellular = cellular, products = "kres", aggregate = FALSE)
    out <- dimSums(out, dim = 3.2)

  } else {
    stop("Product category not avaiable!")
  }

  return(list(x = out,
              weight = NULL,
              unit = "Mt DM, Nr, P, K, WM, Pj Energy",
              description = "Crop Residues Field Production and use",
              min = 0,
              isocountries = !cellular))
}
