#' @title calcDemography
#' @description Population by age, sex and education.
#'
#' With version = "WCDE" the data is taken from the Wittgenstein Centre
#' Human Capital Data Explorer: the historical reconstruction (1965-2015) from
#' version 2 (Lutz et al. 2018) is combined with the updated SSP projections
#' (2020-2100) from version 3 (K.C. et al. 2024). Education is aggregated to the
#' six attainment categories plus "Under 15" that are also used in the legacy
#' Lutz2014 data (the finer post-secondary split of the WCDE data is dropped).
#' With version = "Lutz2014" the previous data source is used instead.
#'
#' Population is divided by sex male (M), female (F) and, before recalibration,
#' both (B), by five-year age groups and by education.
#' @param convert if TRUE, country coverage is completed (convert functions) and
#' the demography is recalibrated to the population scenarios of
#' calcOutput("Population").
#' @param education if FALSE, no education dimension will be provided
#' @param version "WCDE" (default, Wittgenstein Centre v2 history + v3
#' projections) or "Lutz2014" (legacy data source)
#' @export
#' @importFrom magpiesets findset
#'
calcDemography <- function(convert = TRUE, education = TRUE, version = "Lutz2014") {

  past <- magpiesets::findset("past")
  if (version == "Lutz2014") {
  

    lutz <- readSource("Lutz2014", convert = convert)

    mapping2 <- toolGetMapping(type = "sectoral", name = "lutz2hic2.csv",
                               where = "mappingfolder")
    demo <- toolAggregate(x = lutz, rel = mapping2, from = "lutz", to = "hic", dim = 3.2)

    demo <- demo[, , "B", invert = TRUE]
    demo <- demo[, , "All", invert = TRUE]
    demo <- demo[, , "Total", invert = TRUE]

    getSets(demo) <- c("region", "year", "scenario", "sex", "age", "education")

    # in 2010 there are tiny differences in the demography. We still want to have a harmonized
    # dataset for 2010.
    demo[, intersect(getYears(demo), past), ] <- demo[, intersect(getYears(demo), past), "SSP2"]

    # test for differences in population and demography datasets

    if (isTRUE(convert)) {
      population <- calcOutput("Population",
                               scenario = c("SSPs", "SDPs"),
                               years = magpiesets::findset("time"),
                               aggregate = FALSE)
      diff <- dimSums(demo, dim = c("sex", "age", "education")) - population[, getYears(demo), getNames(demo, dim = 1)]
      diff[] <- abs(diff)
      if (sum(diff) > 100) {
        vcat(2, paste0(
          "Population and Demography datasets diverge: ",
          round(mean(dimSums(diff[, intersect(getYears(diff), past), ], dim = c(1, 3)) / dim(diff)[3])),
          " mio per year for the past and ",
          round(mean(dimSums(diff[, setdiff(getYears(diff), past), ], dim = c(1, 3)) / dim(diff)[3])),
          "mio per year for the future. Largest divergences in ",
          where(diff == max(diff))$true$regions[1],
          "in the year ",
          where(diff == max(diff))$true$years[1],
          " with ",
          round(diff[which(diff == max(diff), arr.ind = TRUE)])[1],
          " million people."
        ))
      }

      # recalibration to SSP population scenarios
      # create SDP scenarios columns based on SSP2 and SSP1
      if (any(c("SDP", "SDP_EI", "SDP_MC", "SDP_RC") %in% getNames(population))) {
        demo <- add_columns(demo, addnm = c("SDP", "SDP_EI", "SDP_MC", "SDP_RC"),
                            dim = 3.1, fill = NA)
        demo[, , "SDP", pmatch = TRUE] <- demo[, , "SSP1"]
      }

      demoShr <- demo / dimSums(demo, dim = c("sex", "age", "education"))
      vcat(verbosity = 2, paste0("Year 1965 in demography data missing. Used values of 1970 instead"))
      demoShr <- mbind(
        setYears(demoShr[, "y1970", ], "y1965"),
        demoShr
      )
      demoShr <- toolHoldConstantBeyondEnd(demoShr)
      demo <- demoShr * population
    }

  } else if (version == "WCDE") {

    # education categories to keep (the finer post-secondary split of the WCDE
    # data - Short Post Secondary, Bachelor, Master and higher - is dropped so
    # that the education dimension matches the legacy Lutz2014 scheme)
    keepEdu <- c("Total", "Under 15", "No Education", "Incomplete Primary", "Primary",
                 "Lower Secondary", "Upper Secondary", "Post Secondary")
    sexMapping <- toolGetMapping(type = "sectoral", name = "lutz2hic2.csv",
                                 where = "mappingfolder")

    prepare <- function(subtype) {
      x <- readSource("WCDE", subtype = subtype, convert = convert)
      x <- x[, , keepEdu]
      # sex Male/Female/Both -> M/F/B
      x <- toolAggregate(x, rel = sexMapping, from = "lutz", to = "hic", dim = 3.2)
      x <- x[, , "B", invert = TRUE]
      x <- x[, , "All", invert = TRUE]
      x <- x[, , "Total", invert = TRUE]
      getSets(x) <- c("region", "year", "scenario", "sex", "age", "education")
      return(x)
    }

    historyData <- prepare("epop_v2")   # SSP2 only, 1950-2100 (used for 1965-2015)
    projectionData <- prepare("epop_v3") # SSP1-5, 2020-2100
    scenarios <- getItems(projectionData, dim = "scenario")

    # history (1965-2015) is scenario-invariant and taken from v2 (SSP2),
    # replicated to all scenarios
    historyYears <- getYears(historyData)[getYears(historyData, as.integer = TRUE) %in% 1965:2015]
    historySSP2 <- collapseNames(historyData[, historyYears, "SSP2"])
    history <- mbind(lapply(scenarios, function(s) {
      add_dimension(historySSP2, dim = 3.1, add = "scenario", nm = s)
    }))

    # projections (2020-2100) are taken from v3 per scenario
    projectionYears <- getYears(projectionData)[getYears(projectionData, as.integer = TRUE) >= 2020]
    demo <- mbind(history, projectionData[, projectionYears, scenarios])

    if (isTRUE(convert)) {
      population <- calcOutput("Population",
                               scenario = c("SSPs", "SDPs"),
                               years = magpiesets::findset("time"),
                               aggregate = FALSE)

      # test for differences between raw demography and the population driver
      diff <- dimSums(demo, dim = c("sex", "age", "education")) - population[, getYears(demo), getNames(demo, dim = 1)]
      diff[] <- abs(diff)
      diff[is.na(diff)] <- 0 # ignore countries without historical reconstruction (filled below)
      if (sum(diff) > 100) {
        vcat(2, paste0(
          "Population and Demography datasets diverge: ",
          round(mean(dimSums(diff[, intersect(getYears(diff), past), ], dim = c(1, 3)) / dim(diff)[3])),
          " mio per year for the past and ",
          round(mean(dimSums(diff[, setdiff(getYears(diff), past), ], dim = c(1, 3)) / dim(diff)[3])),
          " mio per year for the future."
        ))
      }

      # create SDP scenario columns based on SSP1
      if (any(c("SDP", "SDP_EI", "SDP_MC", "SDP_RC") %in% getNames(population))) {
        demo <- add_columns(demo, addnm = c("SDP", "SDP_EI", "SDP_MC", "SDP_RC"),
                            dim = 3.1, fill = NA)
        demo[, , "SDP", pmatch = TRUE] <- demo[, , "SSP1"]
      }

      demoShr <- demo / dimSums(demo, dim = c("sex", "age", "education"))

      # Some countries have no historical reconstruction before 2015 in epop_v2
      # (their shares are NA there). Fill the gap by holding the earliest
      # available age-sex-education structure constant (carrying values back in
      # time), analogous to toolHoldConstantBeyondEnd for the future.
      years <- getYears(demoShr)[order(getYears(demoShr, as.integer = TRUE), decreasing = TRUE)]
      for (i in seq_along(years)[-1]) {
        naCells <- is.na(demoShr[, years[i], ])
        if (any(naCells)) {
          reference <- setYears(demoShr[, years[i - 1], ], years[i])
          slice <- demoShr[, years[i], ]
          slice[naCells] <- reference[naCells]
          demoShr[, years[i], ] <- slice
        }
      }

      demoShr <- toolHoldConstantBeyondEnd(demoShr)
      demo <- demoShr * population
    }

  } else {
    stop("Unknown version \"", version, "\" (use \"WCDE\" or \"Lutz2014\")")
  }

  if (!education) {
    demo <- dimSums(demo, dim = "education")
  }

  return(list(
    x = demo,
    weight = NULL,
    min = 0,
    max = 20000,
    unit = "million people",
    description = "Population by age, sex and education",
    isocountries = convert
  ))

}
