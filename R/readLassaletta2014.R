#' @title readLassaletta2014
#' @description reads nitrogen budgets for a country dataset from
#' Lassaletta, L., G. Billen, B. Grizzetti, J. Angalde, and J. Garnier. 2014.
#' 50 Year Trends in Nitrogen Use Efficiency of World Cropping Systems:
#' The Relationship between Yield and Nitrogen Input to Cropland.
#' Environmental Research Letters.
#'
#' @param subtype budget provides the nr cropland budgets, fert_to_cropland
#' the share of inorganic fertilizers being applied to croplands
#' @return Magpie object with results on country level.
#' @author Benjamin Leon Bodirsky, Felicitas Beier
#' @seealso
#' [convertLassaletta2014()],
#' [readSource()]
#' @examples
#' \dontrun{
#' readSource("Lassaletta2014", subtype = "budget", convert = FALSE)
#' }
#' @importFrom readxl read_excel
readLassaletta2014 <- function(subtype = "budget") {
  if (subtype == "budget") {
    out <- NULL
    for (sheetNumber in 3:8) {
      data <- as.data.frame(read_excel("BUDGET_N_Countries_1961_2009_Paper_NUE_Lassaletta_etal_ERL_3.xlsx",
                                       sheet = sheetNumber, progress = FALSE))
      names(data) <- gsub(" ", ".", names(data))
      tmp  <- strsplit(names(data), split = "\\.")
      year <- paste0("y", unlist(lapply(tmp, FUN = function(x) x[length(x)])))
      stopifnot(length(year) >= 2)
      year <- year[2:length(year)]
      indicator <- strsplit(names(data)[2], split = "\\.")[[1]]
      stopifnot(length(indicator) >= 2)
      indicator <- paste(indicator[1:(length(indicator) - 1)], collapse = "_")
      countries <- data[, 1]
      countries <- toolCountry2isocode(countries, mapping = c(`Belgium-Luxemburg` = "BEL"))
      rownames(data) <- countries
      data           <- data[, -1]
      colnames(data) <- year
      data           <- as.magpie(data)
      getNames(data) <- indicator
      out            <- mbind(out, data)
    }
    out <- out * setNames(out[, , "Surfaces_ha"], NULL)
    out <- setNames(out[, , c(
      "Prod_kgN/ha", "Fert_adjusted_grass_kgN/ha", "Manure_kgN/ha", "Fixing_kgN/ha", "Dep_arable_kgN"
    )], c(
      "harvest", "fertilizer", "manure_conf", "fixation_crops", "deposition"
    ))
    out <- out / 10^9
  } else if (subtype == "fert_to_cropland") {
    a <- read.csv(file = "erl502906suppdata1annex.csv", sep = ";", header = 1, row.names = 1)
    dimnames(a)[[2]] <- gsub(dimnames(a)[[2]], pattern = "X", replacement = "y")
    dimnames(a)[[1]] <- toolCountry2isocode(dimnames(a)[[1]], mapping = c(`Belgium-Luxemburg` = "BEL"))
    out <- as.magpie(a)
  } else {
    stop("unknown subtype")
  }
  getSets(out)[1] <- "region"
  out <- clean_magpie(out)
  return(out)
}
