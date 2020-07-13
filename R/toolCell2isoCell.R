#' toolCell2isoCell
#' 
#' Sets cell names to [iso country code].[cell number]
#' @param x magpie object on cellular level
#' @param cells switch between magpie cells (59199) and lpj cells (67420)
#' @return return changed input data
#' @author Kristine Karstens, Felicitas Beier
#' 
#' @importFrom utils read.csv
#' @export

toolCell2isoCell <- function(x,cells="magpiecell"){

  if(cells=="magpiecell"){
    CellToCellIso <- toolGetMapping("CountryToCellMapping.csv",type="cell")
    getCells(x)   <- CellToCellIso$celliso
  } else if(cells=="lpjcell"){
    CellToCellIso <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",type="cell")
    cellNames     <- paste(CellToCellIso$ISO,1:67420,sep=".")
  }

   return(x)
}