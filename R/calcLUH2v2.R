#' @title calcLUH2v2
#' @description Integrates the LUH2v2 landuse-dataset
#'
#' @param landuse_types magpie: magpie landuse classes,
#'                      LUH2v2: original landuse classes
#'                      flooded: flooded areas as reported by LUH
#' @param irrigation    if true: areas are returned separated by irrigated and rainfed,
#'                      if false: total areas
#' @param cellular      if true: dataset is returned on 0.5 degree resolution
#' @param cells         Switch between "magpiecell" (59199) and "lpjcell" (67420)
#' @param selectyears   years to be returned (default: "past")
#'
#' @return List of magpie objects with results on country level,
#'         weight on country level, unit and description
#'
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder, Jens Heinke, Felicitas Beier
#' @seealso
#' \code{\link{calcLanduseInitialisation}}
#' @examples
#' \dontrun{
#' calcOutput("LUH2v2")
#' }
#' @importFrom magclass getNames
#' @importFrom magpiesets findset

calcLUH2v2 <- function(landuse_types = "magpie", irrigation = FALSE,
                       cellular = FALSE, cells = "magpiecell", selectyears = "past") {

  selectyears <- sort(findset(selectyears, noset = "original"))

  if (cellular) {

    x <- readSource("LUH2v2", subtype = "states", convert = "onlycorrect")[, selectyears, ]
    getSets(x, fulldim = FALSE) <- gsub("data", "landuse", getSets(x, fulldim = FALSE))

    if (irrigation) {
      irrigLUH <- readSource("LUH2v2", subtype = "irrigation", convert = "onlycorrect")[, selectyears, ]
    }

  } else {

    x <- readSource("LUH2v2", subtype = "states", convert = TRUE)[, selectyears, ]
    getSets(x) <- c("iso", "t", "landuse")

    if (irrigation) {
      irrigLUH <- readSource("LUH2v2", subtype = "irrigation", convert = TRUE)[, selectyears, ]
    }

  }

  if (irrigation) {

    if (is.null(selectyears)) {
      vcat(verbosity = 3, "too many years may lead to memory problems if irrigation=T")
    }

    # flooded areas
    floodLUH           <- irrigLUH[, , "flood"]

    # irrigated areas (excluding flood)
    irrigLUH           <- irrigLUH[, , "flood", invert = T]
    getNames(irrigLUH) <- substring(getNames(irrigLUH), 7)

    x <- add_dimension(x, dim = 3.2, add = "irrigation", nm = "total")
    x <- add_columns(x, dim = 3.2, addnm = c("irrigated", "rainfed"))
    x[, , "irrigated"] <- 0

    irrigLUH <- add_dimension(irrigLUH, dim = 3.2, add = "irrigation", nm = "irrigated")
    x[, , paste(getNames(irrigLUH, dim = 1), "irrigated", sep = ".")] <- irrigLUH

    # rainfed areas
    x[, , "rainfed"] <- collapseNames(x[, , "total"]) - collapseNames(x[, , "irrigated"])

    if (any(x[, , "rainfed"] < 0)) {
      vcat(verbosity = 2, "Flooded/irrigated area larger than rainfed area.
           Irrigation limited to total cropland area.")
      tmp                       <- collapseNames(x[, , "irrigated"])
      tmp[x[, , "rainfed"] < 0] <- collapseNames(x[, , "total"])[x[, , "rainfed"] < 0]
      x[, , "irrigated"] <- tmp
      x[, , "rainfed"]   <- collapseNames(x[, , "total"]) - collapseNames(x[, , "irrigated"])
    }

    if (any(x[, , "rainfed"] < 0)) {
      vcat(verbositiy = 1, "Flooded/irrigated area larger than rainfed area despite fix.")
    }
  }

  if (landuse_types == "magpie") {

    mapping <- toolGetMapping(type = "sectoral", name = "LUH2v2.csv")
    x       <- toolAggregate(x, mapping, dim = 3.1, from = "luh2v2", to = "land")

  } else if (landuse_types == "LUH2v2") {

    x <- x

  } else if (landuse_types == "flooded") {

    if (irrigation) {

      x <- floodLUH

    } else {
      stop("For landuse_type flood: irrigation argument must be TRUE")
    }

  } else {

    vcat(verbositiy = 1, "non-existant landuse_types")

  }

  # Return correct cell format for further calculations
  if (cellular) {
    x <- toolCoord2Isocell(x, cells = cells)
  }

  #### INCLUDE WHEN READ IS READY (then also include @importFrom mrcommons toolGetMappingCoord2Country)
  # map                   <- toolGetMappingCoord2Country()
  # LUHcroparea           <- LUHcroparea[map$coords,,]
  # getCells(LUHcroparea) <- paste(map$iso, 1:67420, sep=".")

  return(list(x            = x,
              weight       = NULL,
              unit         = "Mha",
              description  = "land area for different land use types.",
              isocountries = !cellular))
}
