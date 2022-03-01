#' @title readZabel2014
#' @description Reads crop suitability data published in Zabel, F., Putzenlechner, B., & Mauser, W. (2014). Global Agricultural Land Resources – A High Resolution Suitability Evaluation and Its Perspectives until 2100 under Climate Change Conditions. PLOS ONE, 9(9), e107522. https://doi.org/10.1371/journal.pone.0107522 and extracts the share of suitable cropland per grid cell, depending on different suitability thresholds.
#' @param subtype The different options are:
#' \itemize{
#' \item \code{"all_marginal"}: Of the total marginal land (suitability index = 0.0 - 0.33), areas with an index of 0.1 and lower are excluded.
#' \item \code{"q33_marginal"}: The bottom tertile (suitability index below 0.13) of the marginal land area is excluded.
#' \item \code{"q50_marginal"}: The bottom  half (suitability index below 0.18) of the marginal land area is excluded.
#' \item \code{"q66_marginal"}: The first and second tertile (suitability index below 0.23) of the marginal land area are excluded.
#' \item \code{"q75_marginal"}: The first, second and third quartiles (suitability index below 0.25) of the marginal land are are excluded
#' \item \code{"no_marginal"}: Areas with a suitability index of 0.33 and lower are excluded.
#' }
#' @return Returns magpie objects with the share of suitable cropland per grid cell
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' readSource("Zabel2014", subtype = "all_marginal", convert = "onlycorrect")
#' }
#'
#' @importFrom raster reclassify
#'

readZabel2014 <- function(subtype = "all_marginal") {

  cropsuitZabel <- raster("./cropsuitability_rainfed_and_irrigated/1981-2010/overall_cropsuit_i_1981-2010/overall_cropsuit_i_1981-2010.tif")

  # define suitability threshold for crop suitability in MAgPIE at original resolution of 30 arc seconds
  # In Zabel et al. (2014) marginal land is defined by a suitability index <= 0.33

  siZabel <- cropsuitZabel

  if (subtype == "all_marginal") {

    # for consistency with older magpie versions this is the same threshold 
    # (suitability index below 0.1) that was applied to the previous
    # data set by Ramankutty et al. (2002)
    rclass_matrx <- matrix(c(
      0, 0.10, 0,
      0.10, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    siZabel <- reclassify(siZabel, rclass_matrx)

  } else if (subtype == "q33_marginal") {

    # The bottom tertile (suitability index below 0.13) of the marginal land area is excluded
    rclass_matrx <- matrix(c(
      0, 0.13, 0,
      0.13, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    siZabel <- reclassify(siZabel, rclass_matrx)

  } else if (subtype == "q50_marginal") {

    # The bottom  half (suitability index below 0.18) of the marginal land area is excluded
    rclass_matrx <- matrix(c(
      0, 0.18, 0,
      0.18, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    siZabel <- reclassify(siZabel, rclass_matrx)

  } else if (subtype == "q66_marginal") {

    # The first and second tertile (suitability index below 0.23) of the marginal land area are excluded
    rclass_matrx <- matrix(c(
      0, 0.23, 0,
      0.23, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    siZabel <- reclassify(siZabel, rclass_matrx)

  } else if (subtype == "q75_marginal") {

    # The first, second and third quartiles (suitability index below 0.25) of the marginal land are are excluded
    rclass_matrx <- matrix(c(
      0, 0.25, 0,
      0.25, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    siZabel <- reclassify(siZabel, rclass_matrx)

  } else if (subtype == "no_marginal") {

    # marginal land (suitability index below 0.33) is fully excluded
    rclass_matrx <- matrix(c(
      0, 0.33, 0,
      0.33, 1, 1
    ),
    ncol = 3, byrow = TRUE
    )
    siZabel <- reclassify(siZabel, rclass_matrx)

  }

  # ignore all NA's and set all land cells to 1 to count total available land cells
  landcellsZabel <- cropsuitZabel
  landcellsZabel <- reclassify(landcellsZabel, cbind(0, 1, 1), include.lowest = TRUE)

  # aggregate and sum up suitable pixels
  # in effect this means counting the pixels that are suitable (1) per 0.5 degree grid cell
  # aggregation factor from 30 arc sec to 0.5 degree: 60
  siZabel_0.5        <- aggregate(siZabel, fact = 60, fun = sum)
  # aggregate and sum to obtain total land cells
  landcellsZabel_0.5 <- aggregate(landcellsZabel, fact = 60, fun = sum, na.rm = TRUE)

  # divide by number of land cells to obtain share of suitable crop land per 0.5 degree cell
  siZabel_share_0.5 <- siZabel_0.5 / landcellsZabel_0.5
  # in some cells -> 0/0=NA
  siZabel_share_0.5[is.na(siZabel_share_0.5)] <- 0

  ### Create magpie object

  # get spatial mapping
  map <- toolGetMappingCoord2Country(pretty = TRUE)
  # transform raster to magpie object
  out <- as.magpie(extract(siZabel_share_0.5, map[c("lon", "lat")]), spatial = 1)
  # set dimension names
  dimnames(out) <- list("x.y.iso" = paste(map$coords, map$iso, sep = "."), "t" = NULL, "data" = paste0("si0_", subtype))

  return(out)
}
