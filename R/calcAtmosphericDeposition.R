#' @title calcAtmosphericDeposition
#' @description Computes Atmospheric (nitrogen) deposition on different land-use types.
#'              It distinguishes ammonia (Nh3) and Nitrogen oxides (NOx) as well
#'
#' @param datasource deposition inventory
#' @param cellular cellular or country level emissions
#' @param cells    magpiecell (59199 cells) or lpjcell (67420 cells)
#' @param emission if TRUE, not the deposition but the cellular emissions are reported
#' @param scenario if dataset contains several scenarios (e.g. ACCMIP), one scenario can be selected.
#' @param glo_incl_oceans provides global values that include oceans, as oceans are not part of the country mapping
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcNitrogenBudgetCropland()]
#' @examples
#' \dontrun{
#' calcOutput("AtmosphericDeposition")
#' }
calcAtmosphericDeposition <- function(datasource = "ACCMIP", glo_incl_oceans = FALSE, # nolint
                                      cellular = FALSE, cells = "lpjcell",
                                      emission = FALSE, scenario = NULL) {

  luhdata <- calcOutput("LanduseInitialisation", cellular = TRUE, cells = "lpjcell", aggregate = FALSE)

  if (is.null(scenario)) {
    scenario <- "rcp45"
  }

  if (datasource %in% c("ACCMIP")) {
    accmip <- calcOutput("ACCMIP", glo_incl_oceans = glo_incl_oceans, aggregate = FALSE)
    if (!emission) {
      accmip2 <- add_dimension(dimSums(accmip[, , c("drydep", "wetdep")][, , c("nh3_n", "no2_n")],
                                       dim = 3.2),
                               dim = 3.2, nm = "deposition")
    } else {
      accmip2 <- accmip[, , c("emi")][, , c("nh3_n", "no2_n")]
    }

    time <- findset("time")
    accmip2 <- time_interpolate(accmip2, interpolated_year = time, integrate_interpolated_years = FALSE,
                                extrapolation_type = "constant")

    if (!glo_incl_oceans) {
      vcat(2, "using constant landuse patterns for future deposition.",
           " Does not affect model results as they will be scaled with area lateron")
      luhdata2 <- toolHoldConstantBeyondEnd(luhdata)
      if (emission) {
        luhdata2 <- dimSums(luhdata2, dim = 3)
      }
      out <- luhdata2 * accmip2
    } else {
      out <- accmip2
    }
    if (!cellular && !glo_incl_oceans) {
      out <- dimSums(out, dim = c("x", "y"))
      out <- toolCountryFill(out, fill = 0, verbosity = 2)
    }
    out <- out[, , scenario]
  } else {
    emi <- calcOutput("EmissionInventory", aggregate = FALSE, datasource = datasource, targetResolution = NULL)
    emi <- dimSums(emi[, , c("nh3_n", "no2_n")], dim = 3.1)
    if (glo_incl_oceans) {
      out <- dimSums(emi, dim = c(1))
    } else {
      redepShare <- calcOutput("AtmosphericRedepositionShare", scenario = scenario,
                               aggregate = FALSE)[, getYears(emi), ]
      transboundaryRedepShare <- calcOutput("AtmosphericTransboundaryRedepositionShare", scenario = scenario,
                                            aggregate = FALSE)[, getYears(emi), ]
      domestic <- emi * redepShare
      transboundary <- dimSums(emi * (1 - dimSums(redepShare, dim = 3.1)), dim = 1) * transboundaryRedepShare
      out <- collapseNames(domestic + transboundary)
      out <- dimOrder(out, perm = c(2, 1))
      if (cellular) {
        weight <- collapseNames(calcOutput("AtmosphericDeposition", datasource = "ACCMIP",
                                           glo_incl_oceans = FALSE, cellular = TRUE, cells = "lpjcell",
                                           emission = FALSE, scenario = NULL, aggregate = FALSE))
        commonCtries <- intersect(getItems(weight, dim = "iso"), getItems(out, dim = 1))
        out <- out[commonCtries, , ]
        getSets(out) <- c("iso", "year", "landuse", "data1")
        weight <- weight[, getItems(out, dim = 2), ]
        weight <- weight[, , getItems(out, dim = 3)]
        coordMapping <- toolGetMappingCoord2Country()
        out <- toolAggregate(out, weight = weight + 10^-10,
                             rel = coordMapping, from = "iso", to = "coords",
                             partrel = FALSE)
        out <- toolCoord2Isocoord(out)
      }
    }

    out <- add_dimension(out, dim = 3.2, nm = "history")
    out <- add_dimension(out, dim = 3.3, nm = "deposition")
  }

  if (any(out < -1e-08)) {
    warning("numbers < -1e-08, check mrcommons:::calcAtmosphericDeposition")
  }
  out[out < 0] <- 0

  # Reduce number of grid cells to 59199
  if (cells == "magpiecell" && cellular) {
    out <- toolCoord2Isocell(out, cells = "magpiecell")
  }

  return(list(x = out,
              weight = NULL,
              unit = "Mt Nr, NH3N and NO2N",
              isocountries = (!cellular & (nregions(out) != 1)),
              min = 0,
              max = 200,
              description = paste0("Atmospheric deposition, natural (1870 levels) and anthropogenic in the ",
                                   "year 1995 (actually 1993) for different landuse classes.")))
}
