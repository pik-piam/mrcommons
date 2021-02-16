#' @title toolSurfaceArea
#' @description Returns surface area information
#'
#' @param level level on which cell area information should be returned ("0.5" for 0.5 degree grid cells, "country"
#' for ISO country level)
#' @return magpie object with surface area in m^2
#' @author Jan Philipp Dietrich
#' @export

toolSurfaceArea <- function(level="0.5") {
  map <- toolGetMappingCoord2Country()
  coords <- matrix(as.numeric(sub("p",".",unlist(strsplit(map$coords,".", fixed=TRUE)))), byrow = TRUE,ncol = 2)
  cell_area  <- (111e3*0.5)*(111e3*0.5)*cos(coords[,2]/180*pi)
  names(cell_area) <- map$coords
  cell_area <- as.magpie(cell_area, spatial = 1)
  getSets(cell_area,fulldim=FALSE) <- c("x.y","time","area")
  if(level=="0.5") return(cell_area)
  if(level=="country") {
    out <- toolAggregate(cell_area,map)
    return(toolCountryFill(out, fill = 0))
  }
  stop("Level \"",level,"\" not supported")
}
