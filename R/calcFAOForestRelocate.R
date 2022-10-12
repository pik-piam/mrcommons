#' @title toolFAOForestRelocate
#' @description Calculates the cellular MAgPIE forest and other land area correction based on FAO forestry data
#' and LUH2v2.
#'
#' @param selectyears default on "past"
#' @param cells    if cellular is TRUE: "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @return List of magpie object with results on cellular level, weight on cellular level, unit and description.
#' @author Kristine Karstens, Felicitas Beier, Patrick v. Jeetze, Jan Philipp Dietrich
#' @importFrom magclass setNames new.magpie nyears
#' @importFrom nleqslv nleqslv
#'
#' @export

toolFAOForestRelocate <- function(luInit, luTargetCountry, vegC) {

  luInitCountry <- toolSum2Country(luInit)


  #Todo: luInit and luTargetCountry are currently 9-class instead of 7-class!!!


  forests <- c("primforest", "secdforest", "forestry")
  nature  <- c(forests, "other") # "primother", "secdother"
  landuse <- c(nature, "urban", "crop", "past")

  # reduce, if nessessary to FAO
  reduce <- increase <- round(luTargetCountry - luInitcountry, 8)
  reduce[reduce > 0]     <- 0
  increase[increase < 0] <- 0

  luInit <- add_columns(luInit, "to_be_allocated", dim = 3.1)
  luInit[, , "to_be_allocated"] <- 0

  # grep land areas dependent on vegetation carbon density
  if (is.null(getYears(vegC))) getYears(vegC) <- getYears(luTargetCountry)

  # weight function to determine correct cellweights for area removal
  findweight <- function(p, cellarea, isoreduction, cellweight) {
    rowSums(cellarea * (1 - (1 - cellweight)^p)) + isoreduction + 10^-10
  }

  # loop over countries
  countries <- getItems(luInit, dim = 1.1)
  for (iso in countries) {

    luiso <- luInit[iso, , ]

    vegCIso <- vegC[iso, , ]

    # normalized vegetation carbon (with small correction to ensure values between [0,1))
    vegCN <- t(as.array(vegCIso / (as.magpie(apply(vegCIso, 2, max)) + 10^-10))[, , 1])

    ###########################
    ### Reduction procedure ###
    ###########################

    # loop over all land use categories, that have to be reallocated
    for (cat in nature) {

      catreduce <- as.array(reduce[iso, , cat])[, , 1]

      # check if area has to be cleared
      if (any(catreduce != 0)) {

        # check for one cell countries
        if (dim(vegCN)[1] == 1) {
          # trivial case of one cell countries
          remove <- -as.magpie(catreduce)
        } else {
          # for other land cell with highest vegc and for all forest categories lowest vegc should be cleared first
          if (cat == "other") {
            cellweight <- vegCN
          } else {
            cellweight <- (1 - 10^-16 - vegCN)
          }

          # check for edge case in which all land of that category must be removed and treat it separately
          fullremoval <- (round(dimSums(luiso, dim = 1)[, , cat] + catreduce, 3) == 0)
          if (any(fullremoval)) {
            luiso[, fullremoval, "to_be_allocated"] <- (luiso[, fullremoval, "to_be_allocated"]
                                                        + setNames(luiso[, fullremoval, cat], NULL))
            luiso[, fullremoval, cat] <- 0
            catreduce[fullremoval] <- 0
          }

          t <- (catreduce != 0)
          if (any(t)) {
            # determine correct parameter for weights for multiple cell countries
            # (weights below zero indicate an error)
            # only determine them for cases where something has to be removed
            p        <- rep(1, nyears(luiso))
            names(p) <- rownames(cellweight)

            for (ti in getYears(luiso[, t, ])) {

              sol  <- nleqslv(rep(1, nyears(luiso[, ti, ])), findweight,
                              cellarea = t(as.array(luiso)[, ti, cat]),
                              isoreduction = catreduce[ti], cellweight = cellweight[ti, ],
                              control = list(allowSingular = TRUE))
              p[ti] <- sol$x
              msg   <- sol$message
              criticalWarnings  <- c("Jacobian is singular (1/condition=0.0e+00) (see allowSingular option)",
                                     "Jacobian is completely unusable (all zero entries?)",
                                     "Iteration limit exceeded")

              if (msg %in% criticalWarnings) {

                vcat(2, paste0("No solution for ", iso, ", ", cat, ", ", msg, ".",
                     "Restart from higher intial guess."))

                sol  <- nleqslv(rep(10^10, nyears(luiso[, ti, ])), findweight,
                                cellarea = t(as.array(luiso)[, ti, cat]),
                                isoreduction = catreduce[ti], cellweight = cellweight[ti, ],
                                control = list(allowSingular = TRUE))
                p[ti] <- sol$x
                msg   <- sol$message
                if (msg %in% criticalWarnings) warning("No solution for ", iso, ", ", cat, ", ", msg, ".")

              }
            }

            if (any(p[t] < 0)) vcat(1, "Negative weight of p=", p, " for: ", cat, " ", iso, " ", t)
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

      cellweight <- (1 - 10^-16 - vegCN)

      # check for one cell countries
      if (dim(vegCN)[1] == 1) {
        # trivial case of one cell countries
        add <- as.magpie(catincrease)
      } else {
        # determine correct parameter for weights for multiple cell countries (weights below zero indicate an error)

        p        <- rep(1, nyears(luiso))
        names(p) <- rownames(cellweight)

        for (ti in getYears(luiso[, t, ])) {

          sol  <- nleqslv(rep(1, nyears(luiso[, ti, ])), findweight,
                          cellarea = t(as.array(luiso)[, ti, "to_be_allocated"]),
                          isoreduction = -catincrease[ti], cellweight = cellweight[ti, ])
          p[ti] <- sol$x
        }

        if (any(p[t] < 0)) vcat(1, "Negative weight of p=", p, " for: ", cat, " ", iso, " ", t)
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

    maxDiff <- max(abs(dimSums(luiso[, , landuse], dim = 1) - luTargetCountry[iso, , landuse]))
    if (maxDiff >= 0.001) {
      tmp <- abs(dimSums(luiso[, , landuse], dim = 1) - luTargetCountry[iso, , landuse])
      luMissmatches <- paste(landuse[unique(which(tmp >= 0.001, arr.ind = TRUE)[, 3])], collapse = ", ")
      warning("Missmatch (", round(maxDiff, 3), " Mha) in ", iso, " for ", luMissmatches)
    }

    luInit[iso, , ] <- luiso

  }

  noCorrLUH2 <- calcOutput("LUH2v2", aggregate = FALSE, landuse_types = "LUH2v2", irrigation = FALSE,
                           cellular = TRUE, selectyears = selectyears, cells = cells, round = 8)

  # calculate shares of primary and secondary non-forest vegetation
  totOtherLuh <- dimSums(noCorrLUH2[, , c("primn", "secdn")], dim = 3)
  primOtherShr <- noCorrLUH2[, , "primn"] / setNames(totOtherLuh + 1e-10, NULL)
  secdOtherShr <- noCorrLUH2[, , "secdn"] / setNames(totOtherLuh + 1e-10, NULL)
  # where luh2 does not report other land, but we find other land after
  # reallocation set share of secondary other land to 1
  secdOtherShr[secdOtherShr == 0 & primOtherShr == 0] <- 1
  # multiply shares of primary and secondary non-forest veg with corrected other land
  primother <- primOtherShr * setNames(luInit[, , "other"], NULL)
  secdother <- secdOtherShr * setNames(luInit[, , "other"], NULL)

  out <- mbind(
    luInit[, , "crop"],
    setNames(noCorrLUH2[, , c("pastr")], "past"),
    setNames(noCorrLUH2[, , c("range")], "range"),
    luInit[, , "urban"],
    luInit[, , forests],
    setNames(primother, "primother"),
    setNames(secdother, "secdother")
  )


  totalArea <- round(dimSums(luInitCountry, dim = c(1, 3)), 3)
  if (!any(round(dimSums(out, dim = c(1, 3)) - totalArea, 3) != 0)) {
    vcat(0, "Something went wrong. Missmatch in total land cover area after reallocation.")
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    description = "Cellular land use initialisation data for different land pools FAO forest corrected",
    isocountries = FALSE
  ))
}
