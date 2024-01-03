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
#'                      NOTE: This setting also affects the sums on country level!
#' @param selectyears   years to be returned (default: "past")
#'
#' @return List of magpie objects with results on country level,
#'         weight on country level, unit and description
#'
#' @author Benjamin Leon Bodirsky, Florian Humpenoeder, Jens Heinke, Felicitas Beier
#' @seealso
#' [calcLanduseInitialisation()]
#' @examples
#' \dontrun{
#' calcOutput("LUH2v2")
#' }
#' @importFrom magclass getNames
#' @importFrom magpiesets findset

calcLUH2v2 <- function(landuse_types = "magpie", irrigation = FALSE, # nolint
                       cellular = FALSE, cells = "lpjcell", selectyears = "past") {

  selectyears <- sort(findset(selectyears, noset = "original"))

  if (!all(landuse_types %in% c("magpie", "LUH2v2", "flooded"))) {
    stop("Unknown lanuses_types = \"", landuse_types, "\"")
  }

  if (landuse_types == "flooded") {
    x <- readSource("LUH2v2", subtype = "irrigation", convert = "onlycorrect")[, selectyears, "flood"]
  } else {
    x <- readSource("LUH2v2", subtype = "states", convert = "onlycorrect")[, selectyears, ]
    getSets(x, fulldim = FALSE)[3] <- "landuse"

    if (isTRUE(irrigation)) {

      irrigLUH <- readSource("LUH2v2", subtype = "irrigation", convert = "onlycorrect")[, selectyears, ]

      if (is.null(selectyears)) {
        vcat(verbosity = 3, "too many years may lead to memory problems if irrigation = TRUE")
      }

      # irrigated areas (excluding flood)
      irrigLUH           <- irrigLUH[, , "flood", invert = TRUE]
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
      mapping <- toolGetMapping(type = "sectoral", name = "LUH2v2.csv", where = "mappingfolder")
      x       <- toolAggregate(x, mapping, dim = 3.1, from = "luh2v2", to = "land")
    }
  }

  # Return correct cell format for further calculations
  # ATTENTION: depending on the settings this might remove some cells
  #            from the data set!
  if (cellular) {
    if (cells == "magpiecell") {
      x <- toolCoord2Isocell(x, cells = cells)
    }
  } else {
    x <- toolConv2CountryByCelltype(x, cells = cells)
  }

  return(list(x            = x,
              weight       = NULL,
              unit         = "Mha",
              description  = "land area for different land use types.",
              isocountries = !cellular))
}
