#' @title readWCDE
#' @description It reads the population projections by age, sex and education
#' of the Wittgenstein Centre Human Capital Data Explorer as downloaded by
#' [downloadWCDE()]. The output format follows [readLutz2014()]:
#' a magpie object with the dimensions country, year, scenario, sex, age and
#' education in million people. In addition to the education categories of
#' Lutz2014, the datasets contain a finer split of the highest levels
#' ("Short Post Secondary", "Bachelor" and "Master and higher"). These belong
#' to a separate classification level and do not sum to "Post Secondary".
#' Age-education combinations that do not occur (e.g. a young child with a
#' university degree) are set to 0, as in Lutz2014. Country-year slices that are
#' entirely absent from the source (the pre-2015 reconstruction is missing for
#' some countries in "epop_v2") are kept as NA. World and continental aggregates
#' (UN M49 codes >= 900) are removed.
#'
#' @param subtype "epop_v3" (projections 2020-2100, K.C. et al. 2024) or
#' "epop_v2" (SSP2 only, including the historical reconstruction 1950-2015,
#' Lutz et al. 2018)
#' @return magpie object with missing countries, which can be filled with
#' the function convertWCDE.
#'
#' @seealso
#' [downloadWCDE()], [convertWCDE()], [readLutz2014()]
#'
#' @importFrom reshape2 acast
readWCDE <- function(subtype = "epop_v3") {
  files <- sort(list.files(pattern = "\\.rds$"))
  if (length(files) == 0) {
    stop("No WCDE .rds files found. The readWCDE function requires a subtype: use ",
         "subtype = \"epop_v3\" (projections 2020-2100) or subtype = \"epop_v2\" ",
         "(historical reconstruction).")
  }
  merge <- list()
  for (file in files) {
    scenario <- sub("\\.rds$", "", file)
    d <- as.data.frame(readRDS(file), stringsAsFactors = FALSE)

    # remove world and continental/regional aggregates (UN M49 codes >= 900)
    d <- d[as.integer(as.character(d$country_code)) < 900, ]

    # change country names to iso codes; the Channel Islands (M49 code 830)
    # have no ISO 3166-1 code and are removed. Unlike Lutz2014, the WCDE
    # datasets contain real data for Taiwan, which is mapped to TWN.
    d$Area <- toolCountry2isocode(d$name, mapping = c("Taiwan Province of China" = "TWN"),
                                  ignoreCountries = "Channel Islands")
    d <- d[!is.na(d$Area), ]

    # add "y" in front of the years
    d$Year <- paste0("y", d$year)

    # transform into magpie object
    out <- acast(d, Area ~ Year ~ sex ~ age ~ education, value.var = "epop")
    out <- as.magpie(out)
    out <- add_dimension(out, dim = 3.1, add = "scenario", nm = scenario)
    merge[[length(merge) + 1]] <- out
  }
  merge <- do.call(mbind, merge)
  getSets(merge) <- c("country", "year", "scenario", "sex", "age", "education")
  merge <- merge / 1000

  # The source omits age-education combinations that do not occur; after
  # reshaping these appear as NA and are set to 0 (no such people), as in
  # Lutz2014. Country-year-scenario slices that are entirely absent from the
  # source (e.g. the pre-2015 reconstruction missing for some countries in
  # epop_v2) are kept as NA so they can be backfilled in calcDemography.
  # The missing slices are captured per scenario before zeroing, because
  # collapseNames would drop the scenario dimension when only one exists.
  scenarios <- getItems(merge, dim = "scenario")
  missByScen <- lapply(scenarios, function(s) {
    gt <- as.array(collapseNames(merge[, , s][, , "Total"][, , "Both"][, , "All"]))
    is.na(gt[, , 1])
  })
  names(missByScen) <- scenarios

  merge[is.na(merge)] <- 0

  for (s in scenarios) {
    miss <- missByScen[[s]]
    for (country in rownames(miss)[apply(miss, 1, any)]) {
      merge[country, colnames(miss)[miss[country, ]], s] <- NA
    }
  }
  return(merge)
}
