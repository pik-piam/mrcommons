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
#' @importFrom SPEI thornthwaite
#' @importFrom luscale weighted_mean.groupAggregate
calcIPCCfracLeach <- function(cellular = TRUE) {
  if (cellular) {
    past <- findset("past")
    # approach based on
    # Velthof, Gerardus Lambertus, and J. Mosquera Losada. 2011. Calculation of Nitrous Oxide Emission
    # from Agriculture in the Netherlands: Update of Emission Factors and Leaching Fraction. Alterra.
    # http://library.wur.nl/WebQuery/wurpubs/406284.
    p <- readSource("LPJml_rev21", "precipitation", convert = FALSE)
    p <- toolCell2isoCell(p)
    t <- readSource("LPJml_rev21", "temperature", convert = FALSE)
    t <- toolCell2isoCell(t)
    lat <- pe <- p
    lat[, , ] <- NA
    pe[, , ] <- NA
    lat <- p[, 1, 1]
    lat[, , ] <- as.numeric(toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")$lat)
    lat <- setNames(setYears(lat, NULL), NULL)

    # estimate potential evapotranspiration using the thornwaite method for temperature and latitude

    tmp <- t
    tmp <- aperm(tmp, c(3, 2, 1))
    old <- tmp
    dim(tmp) <- c(12 * dim(t)[2], dim(t)[1])
    tmp <- thornthwaite(tmp, lat = lat[, , ])
    old[, , ] <- tmp
    pet <- as.magpie(aperm(old, c(3, 2, 1)))

    # komisch, weicht ab wenn man einzelne punkte vergleicht. scheint auch von den
    # nachbarmonaten abzuhängen. pet[2,5,3]==thornthwaite(t[2,1,3],lat=lat[2,,])

    pet <- pet[, past, ]
    prec <- p[, past, ]

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
    lu <- calcOutput("LanduseInitialisation", cellular = TRUE, aggregate = FALSE)

    fracLeachAverage <- lu
    fracLeachAverage[, , ] <- calcOutput("IPCCfracLeach", aggregate = FALSE, cellular = TRUE)

    irrig <- calcOutput("LUH2v2", aggregate = FALSE, cellular = TRUE, irrigation = TRUE)

    irrigShr <- collapseNames(irrig[, , "irrigated"][, , "crop"] / irrig[, , "total"][, , "crop"])
    irrigShr[is.nan(irrigShr)] <- 0

    # set leaching to maximum for irrigated regimes
    fracLeachAverage[, , "crop"] <- fracLeachAverage[, , "crop"] * (1 - irrigShr) + 0.3 * irrigShr

    weight <- lu
    mapping <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")
    fracLeachAverage <- weighted_mean.groupAggregate(data = fracLeachAverage, weight = weight, query = mapping,
                                                       dim = 1, na.rm = TRUE, from = "celliso", to = "iso")
    fracLeachAverage[is.na(fracLeachAverage)] <- 0.05 # mostly forest in desert countries
    fracLeachAverage  <- toolCountryFill(fracLeachAverage, fill = 0.3)
    budget <- calcOutput("NitrogenBudgetCropland", aggregate = FALSE)[, , "surplus"]
    budget2 <- calcOutput("NitrogenBudgetPasture", aggregate = FALSE)[, , "surplus"]
    budget3 <- calcOutput("NitrogenBudgetNonagland", aggregate = FALSE)[, , "surplus"]
    weight <- mbind(setNames(budget, "crop"),
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
