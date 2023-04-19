#' @title calcIPCCfracLeach
#' @description calculates the leaching rate FRAC_LEACH as defined by the IPCC Guidelines for
#' National Greenhouse Gas Inventories 2006. We use the approach used by Canada, see
#' Velthof, Gerardus Lambertus, and J. Mosquera Losada. 2011. Calculation of Nitrous Oxide
#' Emission from Agriculture in the Netherlands: Update of Emission Factors and Leaching Fraction.
#' Alterra. http://library.wur.nl/WebQuery/wurpubs/406284.
#' @param cellular if true, returned on cell level
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("IPCCfracLeach", cellular = FALSE)
#' }
#'
#' @importFrom magpiesets addLocation

calcIPCCfracLeach <- function(cellular = TRUE) {

  if (cellular) {

    past <- findset("past")
    # approach based on
    # Velthof, Gerardus Lambertus, and J. Mosquera Losada. 2011. Calculation of Nitrous Oxide Emission
    # from Agriculture in the Netherlands: Update of Emission Factors and Leaching Fraction. Alterra.
    # http://library.wur.nl/WebQuery/wurpubs/406284.
    # estimate potential evapotranspiration using LPJmL (based on Priestley–Taylor PET model)

    pet    <- calcOutput("LPJmL_new", version = "LPJmL4_for_MAgPIE_44ac93de",
                         climatetype = "GSWP3-W5E5:historical", subtype = "mpet",
                         stage = "smoothed", aggregate = FALSE)[, past, ]
    prec   <- calcOutput("LPJmLClimateInput", lpjmlVersion = "LPJmL4_for_MAgPIE_44ac93de",
                         climatetype  = "GSWP3-W5E5:historical",
                         variable = "precipitation:monthlySum",
                         stage = "smoothed", aggregate = FALSE)[, past, ]

    ratio <- prec / (pet + 0.001)

    fracLeach <- 0.05 + (0.3 - 0.05) / (1 - 0.23) * (ratio - 0.23)
    fracLeach[fracLeach < 0.05] <- 0.05 # minimum leaching
    fracLeach[fracLeach > 0.3] <- 0.3 # maximum leaching
    fracLeach[ratio < 0.1] <- 0 # no leaching without water

    fracLeachAverage <- dimSums(fracLeach[, , ], dim = 3) / 12

    vcat(2, paste("For ", length(which(is.na(fracLeachAverage))),
                  " entries, no PET was possible to compute. set leaching to 0.3"))
    fracLeachAverage[is.na(fracLeachAverage)] <- 0.3

    weight <- NULL

  } else if (!cellular) {

    lu <- calcOutput("LanduseInitialisation", cellular = TRUE, cells = "lpjcell", aggregate = FALSE)
    lu <- dimOrder(collapseDim(addLocation(lu), dim = c("cell")),  perm = c(2, 3, 1), dim = 1)
    names(dimnames(lu))[1] <- "x.y.iso"

    fracLeachAverage   <- lu
    fracLeachAverage[] <- calcOutput("IPCCfracLeach", aggregate = FALSE, cellular = TRUE)

    irrig <- calcOutput("LUH2v2", aggregate = FALSE, cellular = TRUE, cells = "lpjcell", irrigation = TRUE)
    irrig <- dimOrder(collapseDim(addLocation(irrig), dim = c("cell")),  perm = c(2, 3, 1), dim = 1)
    names(dimnames(irrig))[1] <- "x.y.iso"

    irrigShr <- collapseNames(irrig[, , "irrigated"][, , "crop"] / irrig[, , "total"][, , "crop"])
    irrigShr[is.nan(irrigShr)] <- 0

    # set leaching to maximum for irrigated regimes
    fracLeachAverage[, , "crop"] <- fracLeachAverage[, , "crop"] * (1 - irrigShr) + 0.3 * irrigShr

    weight  <- lu
    fracLeachAverage <- toolAggregate(fracLeachAverage, weight = weight, dim = 1, to = "iso")
    fracLeachAverage[is.na(fracLeachAverage)] <- 0.05 # mostly forest in desert countries
    fracLeachAverage  <- toolCountryFill(fracLeachAverage, fill = 0.3)
    budget  <- calcOutput("NitrogenBudgetCropland",  aggregate = FALSE)[, , "surplus"]
    budget2 <- calcOutput("NitrogenBudgetPasture",   aggregate = FALSE)[, , "surplus"]
    budget3 <- calcOutput("NitrogenBudgetNonagland", aggregate = FALSE)[, , "surplus"]
    weight  <- mbind(setNames(budget,  "crop"),
                     setNames(budget2, "past"),
                     setNames(budget3[, , "forestry"], "forestry"),
                     setNames(budget3[, , "primforest"], "primforest"),
                     setNames(budget3[, , "secdforest"], "secdforest"),
                     setNames(budget3[, , "other"], "other"),
                     setNames(budget3[, , "urban"], "urban"))
  }

  return(list(x = fracLeachAverage,
              weight = weight,
              unit = "Million ha",
              min = 0,
              max = 0.31,
              description = "Million hectare land area for different land use types.",
              isocountries = !cellular))
}
