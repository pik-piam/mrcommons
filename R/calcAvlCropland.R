#' @title calcAvlCropland
#'
#' @description Calculates the total available cropland per grid cell,
#'              based on physical cropland suitability data or other criteria,
#'              such as constraints on cropland expansion
#'
#' @param marginal_land different options are
#' \itemize{
#' \item \code{"all_marginal"}: Include all marginal land
#' \item \code{"q33_marginal"}: The bottom tertile of the marginal land area is excluded
#' \item \code{"q50_marginal"}: The bottom  half of the marginal land area is excluded
#' \item \code{"q66_marginal"}: The first and second tertile of the marginal land area are excluded
#' \item \code{"q75_marginal"}: The first, second and third quartiles of the marginal land are are excluded
#' \item \code{"no_marginal"}: Marginal land is fully excluded
#' \item \code{"magpie"}: Returns "all_marginal", "q33_marginal" and "no_marginal" in a magclass object to be used as magpie input.
#' }
#' @param cell_upper_bound Upper bound for cropland at the grid cell level.
#'                         Even if, for instance, the total available cropland area equals the land area in a grid cell,
#'                         cropland cannot be expanded above this value.
#' @param cells            magpiecell (59199 cells) or lpjcell (67420 cells)
#' @param country_level    Whether output shall be at country level.
#'                         Requires aggregate=FALSE in calcOutput.
#'
#' @return magpie object in cellular resolution
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' calcOutput("AvlCropland", marginal_land = "magpie", cells = "magpiecell",
#'            country_level = FALSE, aggregate = FALSE)
#' }
#'
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass dimSums getCells getYears getNames mbind collapseDim as.magpie
#' @importFrom magpiesets addLocation
#'

calcAvlCropland <- function(marginal_land = "magpie", cell_upper_bound = 0.9,
                            cells = "magpiecell", country_level = FALSE) {

  # read luh data
  luh <- calcOutput("LUH2v2", landuse_types = "magpie", aggregate = FALSE,
                    cellular = TRUE, cells = "lpjcell", irrigation = FALSE, years = "y1995")
  # sum land area per grid cell
  luh      <- collapseDim(addLocation(luh), dim = c("N", "cell"))
  landarea <- dimSums(luh, dim = 3)
  # calculate crop share in the land use initialisation data
  cropShrLUH <- luh[, , "crop"] / landarea
  cropShrLUH[is.na(cropShrLUH)] <- 0

  x <- as.magpie(NULL)

  if (any(grepl("all_marginal", marginal_land)) | marginal_land == "magpie") {

    cropsuit <- readSource("Zabel2014", subtype = "all_marginal", convert = "onlycorrect")
    # set upper bound for cropland at grid cell level in each
    # grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, cropShrLUH)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuitArea <- cropsuit * landarea

    tmp <- cropsuitArea
    getNames(tmp) <- "all_marginal"
    x <- mbind(x, tmp)

  }

  if (any(grepl("q33_marginal", marginal_land)) | marginal_land == "magpie") {

    cropsuit <- readSource("Zabel2014", subtype = "q33_marginal", convert = "onlycorrect")
    # set upper bound for cropland at grid cell level in each
    # grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, cropShrLUH)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuitArea <- cropsuit * landarea

    tmp <- cropsuitArea
    getNames(tmp) <- "q33_marginal"
    x <- mbind(x, tmp)

  }

  if (any(grepl("q50_marginal", marginal_land))) {

    cropsuit <- readSource("Zabel2014", subtype = "q50_marginal", convert = "onlycorrect")
    # set upper bound for cropland at grid cell level in each
    # grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, cropShrLUH)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuitArea <- cropsuit * landarea

    tmp <- cropsuitArea
    getNames(tmp) <- "q50_marginal"
    x <- mbind(x, tmp)

  }


  if (any(grepl("q66_marginal", marginal_land))) {

    cropsuit <- readSource("Zabel2014", subtype = "q66_marginal", convert = "onlycorrect")
    # set upper bound for cropland at grid cell level in each
    # grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, cropShrLUH)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuitArea <- cropsuit * landarea

    tmp <- cropsuitArea
    getNames(tmp) <- "q66_marginal"
    x <- mbind(x, tmp)

  }

  if (any(grepl("q75_marginal", marginal_land))) {

    cropsuit <- readSource("Zabel2014", subtype = "q75_marginal", convert = "onlycorrect")
    # set upper bound for cropland at grid cell level in each
    # grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, cropShrLUH)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuitArea <- cropsuit * landarea

    tmp <- cropsuitArea
    getNames(tmp) <- "q75_marginal"
    x <- mbind(x, tmp)

  }

  if (any(grepl("no_marginal", marginal_land)) | marginal_land == "magpie") {

    cropsuit <- readSource("Zabel2014", subtype = "no_marginal", convert = "onlycorrect")
    # set upper bound for cropland at grid cell level in each
    # grid cell cropland cannot be expanded above this threshold
    cropsuit <- cropsuit * cell_upper_bound
    # cropland suitability is corrected where LUH reports (more) cropland
    cropsuit <- pmax(cropsuit, cropShrLUH)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuitArea <- cropsuit * landarea

    tmp <- cropsuitArea
    getNames(tmp) <- "no_marginal"
    x <- mbind(x, tmp)

  }


  if (country_level) {

    out <- toolAggregateCell2Country(collapseDim(x, dim = "iso"), fill = 0)

  } else {

    if (cells == "magpiecell") {

      out <- toolCoord2Isocell(x)

    } else if (cells == "lpjcell") {

      out <- x

    } else {
      stop("Please specify cells argument")
    }
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "Cropland suitability based on Zabel et al. (2014) with different suitability thresholds ('all_marginal', 'half_marginal', 'no_marginal').",
              isocountries = FALSE))
}
