#' @title calcFAOForestRelocate
#' @description Calculates the cellular MAgPIE forest and other land area correction based on FAO forestry data
#' and LUH2v2.
#'
#' @param selectyears default on "past"
#' @param nclasses options are either "six", "seven" or "nine".
#' \itemize{
#' \item "six" includes the original land use classes "crop", "past", "forestry", "forest", "urban" and "other"
#' \item "seven" separates primary and secondary forest and includes "crop", "past", "forestry", "primforest",
#' "secdforest", "urban" and "other"
#' \item "nine" adds the separation of pasture and rangelands, as well as a differentiation of primary and
#' secondary non-forest vegetation and therefore returns "crop", "past", "range", "forestry", "primforest",
#' "secdforest", "urban", "primother" and "secdother"
#' }
#' @param cells    if cellular is TRUE: "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @return List of magpie object with results on cellular level, weight on cellular level, unit and description.
#' @author Kristine Karstens, Felicitas Beier, Patrick v. Jeetze
#' @examples
#' \dontrun{
#' calcOutput("FAOForestRelocate")
#' }
#' @importFrom magclass setNames new.magpie nyears
#' @importFrom nleqslv nleqslv
#'
#' @export

calcFAOForestRelocate <- function(selectyears = "past", nclasses = "seven", cells = "magpiecell") {

  # Load cellular and country data
  countrydata <- calcOutput("LanduseInitialisation", aggregate = FALSE, nclasses = "seven", fao_corr = TRUE,
                            selectyears = selectyears, cellular = FALSE)
  LUH2v2Init <- calcOutput("LanduseInitialisation", aggregate = FALSE, nclasses = "seven", fao_corr = FALSE, # nolint
                           selectyears = selectyears, cellular = TRUE, cells = cells)

  totalarea <- dimSums(LUH2v2Init, dim = c(1, 3))

  if (cells == "lpjcell") {
    mapping   <- toolGetMappingCoord2Country()
    cellvegc  <- calcOutput("LPJmL_new", version = "LPJmL4_for_MAgPIE_44ac93de", climatetype = "GSWP3-W5E5:historical",
                            subtype = "vegc", stage = "smoothed", aggregate = FALSE)[, getYears(countrydata), ]
    countries <- unique(gsub("[^A-Z]", "", getCells(cellvegc)))
    getCells(LUH2v2Init) <-  paste(gsub("[^A-Z]", "", getCells(cellvegc)), c(1:67420), sep = ".")
    getCells(cellvegc)    <-  paste(gsub("[^A-Z]", "", getCells(cellvegc)), c(1:67420), sep = ".")
    names(dimnames(LUH2v2Init))[1] <- "celliso"
    names(dimnames(cellvegc))[1]    <- "celliso"
    mapping   <- data.frame(mapping, celliso = paste(gsub("[^A-Z]", "", getCells(cellvegc)), c(1:67420), sep = "."),
                            stringsAsFactors = FALSE)
  } else {
    mapping   <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")
    countries <- unique(mapping$iso)

    cellvegc <- calcOutput("LPJmL_new", version = "LPJmL4_for_MAgPIE_44ac93de", climatetype = "GSWP3-W5E5:historical",
                           subtype = "vegc", stage = "smoothed", aggregate = FALSE)[, getYears(countrydata), ]
    # reduce to 59199 cells and rename cells
    cellvegc <- toolCoord2Isocell(cellvegc)
  }

  forests <- c("primforest", "secdforest", "forestry")
  nature  <- c(forests, "other")
  landuse <- c(nature, "urban", "crop", "past")

  # reduce, if nessessary to FAO
  reduce <- increase <- round(countrydata - toolCountryFill(toolAggregate(LUH2v2Init, rel = mapping, from = "celliso",
                                                                          to = "iso", partrel = TRUE), fill = 0), 8)
  reduce[reduce > 0]     <- 0
  increase[increase < 0] <- 0

  LUH2v2Init <- add_columns(LUH2v2Init, "to_be_allocated", dim = 3.1) # nolint
  LUH2v2Init[, , "to_be_allocated"] <- 0

  # grep land areas dependent on vegetation carbon density
  if (is.null(getYears(cellvegc))) getYears(cellvegc) <- getYears(countrydata)

  # weight function to determine correct cellweights for area removal
  findweight <- function(p, cellarea, isoreduction, cellweight) {
    rowSums(cellarea * (1 - (1 - cellweight)^p)) + isoreduction + 10^-10
  }

  # loop over countries and years
  for (iso in countries) {

    luiso <- LUH2v2Init[iso, , ]

    cveg <- cellvegc[iso, , ]

    # normalized vegetation carbon (with small correction to ensure values between [0,1))
    cellVegcN <- t(as.array(cveg / (as.magpie(apply(cveg, 2, max)) + 10^-10))[, , 1])

    ###########################
    ### Reduction procedure ###
    ###########################

    # loop over all land use categories, that has to be reallocated
    for (cat in nature) {

      catreduce <- as.array(reduce[iso, , cat])[, , 1]

      # check if area has to be cleared
      if (any(catreduce != 0)) {

        # check for one cell countries
        if (dim(cellVegcN)[1] == 1) {
          # trivial case of one cell countries
          remove <- -as.magpie(catreduce)
        } else {
          # for other land cell with highest vegc and for all forest categories lowest vegc should be cleared first
          if (cat == "other") {
            cellweight <- cellVegcN
          } else {
            cellweight <- (1 - 10^-16 - cellVegcN)
          }

          # check for edge case in which all land of that category must be removed and treat it separately
          fullremoval <- (round(dimSums(luiso, dim = 1)[, , cat] + catreduce, 2) == 0)
          if (any(fullremoval)) {
            luiso[, fullremoval, "to_be_allocated"] <- (luiso[, fullremoval, "to_be_allocated"]
                                                        + setNames(luiso[, fullremoval, cat], NULL))
            luiso[, fullremoval, cat] <- 0
            catreduce[fullremoval] <- 0
          }

          t <- (catreduce != 0)
          if (any(t)) {
            # determine correct parameter for weights for multiple cell countries (weights below zero indicate an error)
            p <- nleqslv(rep(1, nyears(luiso)), findweight, cellarea = t(as.array(luiso)[, , cat]),
                         isoreduction = catreduce, cellweight = cellweight)$x
            names(p) <- rownames(cellweight)
            if (any(p[t] < 0)) vcat(2, "Negative weight of p=", p, " for: ", cat, " ", iso, " ", t)
            remove <- luiso[, , cat] * (1 - (1 - as.magpie(cellweight))^as.magpie(p))
            remove[, !t, ] <- 0
          } else {
            remove <- 0
          }
        }

        # remove area from cells and put to "to_be_allocated" area
        luiso[, , cat] <- luiso[, , cat] - remove
        luiso[, , "to_be_allocated"] <- luiso[, , "to_be_allocated"] + remove
      }
    }

    ############################
    ### Allocation procedure ###
    ############################

    catincrease <- as.array(increase[iso, , "other"])[, , 1]

    # relocate other land to areas with low vegetation carbon density
    # check if other land has to be filled
    if (any(catincrease != 0)) {

      t <- (catincrease != 0)

      cellweight <- (1 - 10^-16 - cellVegcN)


      # check for one cell countries
      if (dim(cellVegcN)[1] == 1) {
        # trivial case of one cell countries
        add <- as.magpie(catincrease)
      } else {
        # determine correct parameter for weights for multiple cell countries (weights below zero indicate an error)
        p <- nleqslv(rep(1, nyears(luiso)), findweight, cellarea = t(as.array(luiso)[, , "to_be_allocated"]),
                     isoreduction = -catincrease, cellweight = cellweight)$x
        names(p) <- rownames(cellweight)
        if (any(p[t] < 0)) vcat(2, "Negative weight of p=", p, " for: ", cat, " ", iso, " ", t)
        add <- luiso[, , "to_be_allocated"] * (1 - (1 - as.magpie(cellweight))^as.magpie(p))
      }
      add[, !t, ] <- 0

      # move area from "to_be_allocated" area to other land
      luiso[, , "other"] <- luiso[, , "other"] + add
      luiso[, , "to_be_allocated"] <- luiso[, , "to_be_allocated"] - add
    }

    # relocate forest land to remaining "to_be_allocated" area
    # check if forests has to be filled

    catincrease <- increase[iso, , forests]

    if (any(catincrease != 0)) {

      # move area from "to_be_allocated" area to forests
      forestsShare <- catincrease / (setNames(dimSums(catincrease, dim = 3), NULL) + 10^-10)
      luiso[, , forests] <- (luiso[, , forests] + setCells(forestsShare, "GLO")
                             * setNames(luiso[, , "to_be_allocated"], NULL))

      luiso[, , "to_be_allocated"] <- 0
    }

    ############################
    ### Check reallocation   ###
    ############################

    maxDiff <- max(abs(dimSums(luiso[, , landuse], dim = 1) - countrydata[iso, , landuse]))
    if (maxDiff >= 0.001) {
      warning("Missmatch in data for ", iso, " (maxDiff = ", maxDiff, "Mha)")
    }

    LUH2v2Init[iso, , ] <- luiso

  }

  if (nclasses == "nine") {
    LUH2v2NoCorr <- calcOutput("LUH2v2", aggregate = FALSE, landuse_types = "LUH2v2", irrigation = FALSE, # nolint
                                cellular = TRUE, selectyears = selectyears, cells = cells, round = 8)

    # calculate shares of primary and secondary non-forest vegetation
    totOtherLuh <- dimSums(LUH2v2NoCorr[, , c("primn", "secdn")], dim = 3)
    primOtherShr <- LUH2v2NoCorr[, , "primn"] / setNames(totOtherLuh + 1e-10, NULL)
    secdOtherShr <- LUH2v2NoCorr[, , "secdn"] / setNames(totOtherLuh + 1e-10, NULL)
    # where luh2 does not report other land, but we find other land after
    # reallocation set share of secondary other land to 1
    secdOtherShr[secdOtherShr == 0 & primOtherShr == 0] <- 1
    # multiply shares of primary and secondary non-forest veg with corrected other land
    primother <- primOtherShr * setNames(LUH2v2Init[, , "other"], NULL)
    secdother <- secdOtherShr * setNames(LUH2v2Init[, , "other"], NULL)

    out <- mbind(
      LUH2v2Init[, , "crop"],
      setNames(LUH2v2NoCorr[, , c("pastr")], "past"),
      setNames(LUH2v2NoCorr[, , c("range")], "range"),
      LUH2v2Init[, , "urban"],
      LUH2v2Init[, , forests],
      setNames(primother, "primother"),
      setNames(secdother, "secdother")
    )
  } else if (nclasses == "seven") {
    out <- LUH2v2Init[, , landuse]
  } else if (nclasses == "six") {
    out <- mbind(
      LUH2v2Init[, , "crop"],
      LUH2v2Init[, , "past"],
      LUH2v2Init[, , "urban"],
      LUH2v2Init[, , "forestry"],
      setNames(LUH2v2Init[, , "primforest"] + LUH2v2Init[, , "secdforest"], "forest"),
      LUH2v2Init[, , "other"]
    )
  }

  if (!any(round(dimSums(out, dim = c(1, 3)) - round(totalarea, 3), 3) != 0)) {
    vcat(2, "Something went wrong. Missmatch in total land cover area after reallocation.")
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    description = "Cellular land use initialisation data for different land pools FAO forest corrected",
    isocountries = FALSE
  ))
}
