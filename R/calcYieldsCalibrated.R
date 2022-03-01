#' @title calcYieldsCalibrated
#' @description This functions calibrates extracted yields from LPJmL to
#'              FAO country level yields
#'
#' @param source      Defines LPJmL version for main crop inputs and isimip replacement.
#'                    For isimip choose crop model/gcm/rcp/co2 combination formatted like this:
#'                    "yields:EPIC-IIASA:ukesm1-0-ll:ssp585:default:3b"
#' @param climatetype switch between different climate scenarios
#' @param refYear     reference year for calibration
#' @param cells       number of cells "magpiecell" for 59199 cells or
#'                    "lpjcell" for 67420 cells
#'
#' @return magpie object in cellular resolution from reference year onwards
#'
#' @author Kristine Karstens, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("YieldsCalibrated", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset
#' @importFrom magclass getYears getNames dimSums mbind
#' @importFrom madrat calcOutput toolConditionalReplace

calcYieldsCalibrated <- function(source = c(lpjml = "ggcmi_phase3_nchecks_9ca735cb", isimip = NULL),
                                 climatetype = "GSWP3-W5E5:historical", refYear = "y1995", cells = "magpiecell") {

    sizelimit <- getOption("magclass_sizeLimit")
    options(magclass_sizeLimit = 1e+12)
    on.exit(options(magclass_sizeLimit = sizelimit))

    # correct ref year format
    if (!grepl("y", refYear)) {
      refYear <- paste0("y", refYear)
    }

    # extract crop list
    crops          <- setdiff(findset("kcr"), c("betr", "begr"))

    # read FAO and LPJmL yields
    yieldFAOiso    <- calcOutput("FAOYield", cut = 0.98, aggregate = FALSE)[, refYear, crops]
    yieldLPJmLgrid <- calcOutput("Yields", source = source, climatetype = climatetype,
                                  aggregate = FALSE, supplementary = TRUE, cells = cells)

    years          <- getYears(yieldLPJmLgrid$x, as.integer = TRUE)
    years          <- years[years >= as.integer(gsub("y", "", refYear))]
    weight         <- yieldLPJmLgrid$weight

    otherYields    <- yieldLPJmLgrid$x[, years, setdiff(getNames(yieldLPJmLgrid$x, dim = 1), crops)]
    yieldLPJmLgrid <- yieldLPJmLgrid$x[, years, crops]

    # adjust cell naming
    if (cells == "lpjcell") {
      yieldLPJmLgrid <- toolCoord2Isocell(yieldLPJmLgrid, cells = cells)
    }

    # crop-specific cropland area split by irrigation and rainfed
    cropareaMAGgrid <- calcOutput("Croparea", sectoral = "kcr", physical = TRUE,
                                  cellular = TRUE,  cells = cells,
                                  irrigation = TRUE, aggregate = FALSE)[, refYear, crops]

    # total irrigated & rainfed cropland (for correction of 0 cropland areas)
    proxyMAGgrid    <- dimSums(cropareaMAGgrid, dim = "MAG")

    # Aggregate to country values
    # Crop-specific total cropland area per country
    cropareaMAGiso  <- dimSums(cropareaMAGgrid, dim = c("cell", "irrigation"))

    # Averaged LPJmL yield per country (LPJmL production / area)
    yieldLPJmLiso   <- dimSums(dimSums(yieldLPJmLgrid[, refYear, ] * cropareaMAGgrid,
                                       dim = 3.2),
                               dim = "cell") / cropareaMAGiso

    # Correction where no historical crop-specific areas given
    yieldLPJmLiso[cropareaMAGiso == 0] <- (dimSums(dimSums(yieldLPJmLgrid[, refYear, ] * proxyMAGgrid,
                                                           dim = 3.2),
                                                   dim = "cell") / dimSums(cropareaMAGiso,
                                                                           dim = 3))[cropareaMAGiso == 0]

    # Correction NAs
    yieldLPJmLiso <- toolConditionalReplace(yieldLPJmLiso, "is.na()", 0)

    # Harmonize countries
    yieldLPJmLiso <- yieldLPJmLiso[intersect(getCells(yieldLPJmLiso), getCells(yieldFAOiso)), , ]
    yieldFAOiso   <- yieldFAOiso[intersect(getCells(yieldLPJmLiso), getCells(yieldFAOiso)), , ]

    # Yield calibration of LPJmL yields to FAO country yield levels
    out <- toolPatternScaling(yieldLPJmLgrid, yieldLPJmLiso, yieldFAOiso, ref_year = refYear)

    # Combine with pasture, betr, begr yields that were not calibrated
    getCells(out) <- getCells(otherYields)
    out           <- mbind(out, otherYields)

    return(list(x            = out,
                weight       = weight,
                unit         = "t DM per ha physical area",
                description  = "Calibrated crop yields by plant type and irrigation",
                isocountries = FALSE))
}
