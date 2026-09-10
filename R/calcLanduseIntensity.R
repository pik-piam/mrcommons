#' @title calcLanduseIntensity
#'
#' @description This function prepares total tau values for use. As the source data already
#'              provides all required information this function purely removes unrequired
#'              data and moves the xref values to the weighting object which is required for
#'              aggregation.
#'
#' @param rescale  TRUE (default), if Tau should be rescaled in a way, that 2010 is always 1
#' @param sectoral "kcr" (default) for MAgPIE crop items and "lpj" for LPJmL crop items, "pasture" for pasture
#' @return Total tau data and corresponding weights as a list of two MAgPIE
#' objects
#' @author Benjamin Leon Bodirsky, Kristine Karstens, Felicitas Beier
#' @seealso [madrat::calcTauTotal()], [madrat::readTau()],
#' [madrat::convertTau()]
#' @examples
#' \dontrun{
#' calcOutput("LanduseIntensity")
#' }
#'
#' @importFrom madrat toolAggregate

calcLanduseIntensity <- function(sectoral = "kcr", rescale = TRUE) {

  if (sectoral %in% c("kcr", "lpj")) {
    # Mappings
    cropsMAgPIE  <- findset("kcr")
    mag2lpj      <- toolGetMapping(name = "MAgPIE_LPJmL.csv", type = "sectoral",
                                   where = "mrlandcore")
    mag2lpj      <- mag2lpj[mag2lpj$MAgPIE %in% cropsMAgPIE, ]
    cropsLPJmL   <- levels(droplevels(factor(mag2lpj$LPJmL5)))

    # Load LPJ yields and area on cell level
    # croparea
    cropareaLPJmL   <- calcOutput("Croparea", sectoral = sectoral, physical = TRUE,
                                  cellular = TRUE, irrigation = TRUE, aggregate = FALSE)

    # HACKATHON: In calcProduction we now set this to calcYieldsLPJmL.
    # I'm actually not sure what is better. See comments there about multiple cropping argument.
    # We should decide for one way and do hte same here and in calcProduction
    # and all other instances.
    cfgLPJmL     <- mrlandcore::toolLPJmLDefault(suppressNote = FALSE)

    # LPJmL yields with default settings
    # Note: When not all years of cropareaMAG are included in the historical data
    # of LPJmL, calcOutput returns a warning that is here suppressed, since
    # the years are in this case filled with toolHoldConstant
    yieldsLPJmL <- suppressWarnings(collapseNames(calcOutput("YieldsLPJmL", lpjml = cfgLPJmL$defaultLPJmLVersion,
                                                             climatetype = cfgLPJmL$baselineHist,
                                                             selectyears = getItems(cropareaLPJmL, dim = 2),
                                                             aggregate = FALSE)[, , cropsLPJmL]))
    # Note (for multiple cropping implementation): I did not set multiple cropping
    # argument (default now: multicropping = FALSE)
    # It then just goes to the default, so once we activate multiple cropping it would
    # be multiple cropping yields (where it currently happens), but we should
    # make sure it's only set to "historical" multiple cropping, never accidentally to
    # "future/potential" multiple cropping.

    # extend years
    yieldsLPJmL <- toolHoldConstant(yieldsLPJmL, years = getItems(cropareaLPJmL, dim = 2))

    if (sectoral == "kcr") {
      yieldsLPJmL   <- toolAggregate(yieldsLPJmL, rel = mag2lpj,
                                     from = "LPJmL5", to = "MAgPIE", dim = "crop")
    }

    commonYears     <- sort(intersect(getYears(cropareaLPJmL), getYears(yieldsLPJmL)))
    cropareaLPJmL   <- cropareaLPJmL[, commonYears, ]
    yieldsLPJmL     <- yieldsLPJmL[, commonYears, ]

    productionLPJmL <- yieldsLPJmL[, commonYears, ] * cropareaLPJmL[, commonYears, ]
    # Aggregate to countries and across irrigation dimension
    productionLPJmL <- dimSums(productionLPJmL, dim = c("x", "y", "irrigation"))

    # Load FAO data and caluculate FAO yields on country level
    productionFAO   <- collapseNames(calcOutput("FAOmassbalance",
                                                aggregate = FALSE)[, , "production"][, , "dm"][, , cropsMAgPIE])

    commonYears     <- sort(intersect(getYears(productionFAO), getYears(productionLPJmL)))
    productionLPJmL <- productionLPJmL[, commonYears, ]
    productionFAO   <- productionFAO[, commonYears, ]
    cropareaLPJmL   <- cropareaLPJmL[, commonYears, ]

    if (sectoral == "lpj") {
      productionFAO <- toolAggregate(productionFAO, rel = mag2lpj,
                                     from = "MAgPIE", to = "LPJmL5", dim = 3.1)
    }

    # Getting overlapping countries
    regions          <- intersect(getItems(productionLPJmL, dim = 1.1),
                                  getItems(productionFAO, dim = 1.1))
    productionLPJmL  <- productionLPJmL[regions, , ]
    productionFAO    <- productionFAO[regions, , ]

    # Calculate TAU as ratio of FAO to LPJmL yields
    tau              <- productionFAO / productionLPJmL
    tau[is.na(tau)]  <- 0
    tau[tau == Inf]  <- 0

    # Aggregate to countries and across irrigation dimension
    cropareaCountry  <- dimSums(cropareaLPJmL, dim = c(1.1, 1.2, 3.1))

    # rescale such that average in 2010 is 1
    if (rescale) {
      rescale2010   <- toolNAreplace(x = tau[, "y2010", ],
                                     weight = cropareaCountry[getItems(tau, dim = 1.1), "y2010", ])
      rescaleWeight <- dimSums(rescale2010$x * rescale2010$weight,
                               dim = 1) / dimSums(rescale2010$weight,
                                                  dim = 1)
      tau           <- tau / setYears(rescaleWeight, NULL)
      tau[is.na(tau)]  <- 0
    }

    # calculate TAU aggregated over all croptypes
    kcr2all <- matrix(c(cropsMAgPIE, rep("all", length(cropsMAgPIE))),
                      ncol = 2,
                      dimnames = list(NULL, c("kcr", "all")))
    tauall  <- toolAggregate(tau, rel = kcr2all, weight = cropareaCountry + 10^(-10), from = "kcr", to = "all", dim = 3)

    x      <- mbind(tau, setNames(tauall, "all"))
    weight <- cropareaCountry
    weight <- mbind(weight, setNames(dimSums(weight, dim = 3.1, na.rm = TRUE), "all"))
    out    <- toolNAreplace(x = x, weight = weight)
    x      <- toolCountryFill(out$x, fill = 0)
    weight <- toolCountryFill(out$weight, fill = 0)
    #  ?Old comment: if only one indicator is required over all crops, I suggest a weighting over area harvested

  } else if (sectoral == "pasture") {

    # read in pasture area
    pastareaMAgPIE <- collapseNames(calcOutput("LanduseInitialisation",
                                               cellular = TRUE,
                                               aggregate = FALSE)[, , "past"])

    # Load LPJ yields and area on cell level
    cfgLPJmL     <- mrlandcore::toolLPJmLDefault(suppressNote = FALSE)
    yieldsLPJmL  <- suppressWarnings(collapseNames(calcOutput("LPJmLTransform",
                                                              years = getItems(pastareaMAgPIE, dim = 2),
                                                              lpjmlversion = cfgLPJmL$defaultLPJmLVersion,
                                                              climatetype = cfgLPJmL$baselineHist,
                                                              subtype = "cropsRF:pft_harvestc",
                                                              stage = "smoothed:cut",
                                                              aggregate = FALSE)[, , "rainfed"][, , "grassland"]))
    # extend years to all past
    yieldsLPJmL <- toolHoldConstant(yieldsLPJmL, years = getItems(pastareaMAgPIE, dim = 2))

    getNames(yieldsLPJmL) <- getNames(pastareaMAgPIE) <- "pasture"

    commonYears           <- sort(intersect(getYears(pastareaMAgPIE), getYears(yieldsLPJmL)))
    pastareaMAgPIE        <- pastareaMAgPIE[, commonYears, ]
    yieldsLPJmL           <- yieldsLPJmL[, commonYears, ]

    productionLPJmL  <- yieldsLPJmL[, commonYears, ] * pastareaMAgPIE[, commonYears, ]
    # Aggregate to country values
    productionLPJmL  <- dimSums(productionLPJmL, dim = c("x", "y"))

    # Load FAO data and caluculate FAO yields on country level
    productionFAO    <- collapseNames(calcOutput("FAOmassbalance",
                                                 aggregate = FALSE)[, , "production"][, , "dm"][, , "pasture"])

    commonYears     <- sort(intersect(getYears(productionFAO), getYears(productionLPJmL)))
    productionLPJmL <- productionLPJmL[, commonYears, ]
    productionFAO   <- productionFAO[, commonYears, ]
    pastareaMAgPIE  <- pastareaMAgPIE[, commonYears, ]

    # Getting overlapping countries
    regions          <- intersect(getItems(productionLPJmL, dim = 1.1),
                                  getItems(productionFAO, dim = 1.1))
    productionLPJmL  <- productionLPJmL[regions, , ]
    productionFAO    <- productionFAO[regions, , ]

    # Calculate TAU as ratio of FAO to LPJmL yields
    tau              <- productionFAO / productionLPJmL
    tau[is.na(tau)]  <- 0
    tau[tau == Inf]  <- 0

    # Aggregate to country data
    pastareaCountry  <- dimSums(pastareaMAgPIE, dim = c("x", "y"))

    # rescale such that average in 2010 is 1
    if (rescale) {
      rescale2010   <- toolNAreplace(x = tau[, "y2010", ],
                                     weight = pastareaCountry[getItems(tau, dim = 1.1), "y2010", ])
      rescaleWeight <- dimSums(rescale2010$x * rescale2010$weight, dim = 1) / dimSums(rescale2010$weight, dim = 1)
      tau           <- tau / setYears(rescaleWeight, NULL)
    }

    x      <- tau
    weight <- pastareaCountry[getItems(tau, dim = 1.1), getYears(tau), ]
    out    <- toolNAreplace(x = x, weight = weight)
    x      <- toolCountryFill(out$x, fill = 0)
    weight <- toolCountryFill(out$weight, fill = 0)

  } else {
    stop("selected sectoral setting in calcLanduseIntensity not possible (for now)!")
  }

  return(list(x           = x,
              weight      = weight,
              unit        = "",
              description = "FAO production divided by LPJml yield
                             times LUH areas for MAgPIE representative crops and pasture",
              note        = c("")))
}
