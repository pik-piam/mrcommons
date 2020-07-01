#' toolIso2CellCountries
#' 
#' Select country names of countries which are present on cellular level
#' @param x magpie object on iso country level
#' @param cells switch between 59199 ("magpiecell") and 67420 ("lpjcell") cells
#' @return return selected input data
#' @author Kristine Karstens, Felicitas Beier
#' 
#' @importFrom utils read.csv
#' @export


toolIso2CellCountries <- function(x,cells="magpiecell"){
  
  if(cells=="magpiecell"){
    CellToCellIso <- toolGetMapping("CountryToCellMapping.csv",type="cell")
    IsoCellCountry <- unique(CellToCellIso$iso)
  } else if(cells=="lpjcell"){
    CellToCellIso  <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",type="cell")
    IsoCellCountry <- unique(CellToCellIso$ISO)
    IsoCellCountry <- IsoCellCountry[-c(grep("XNL",IsoCellCountry),grep("KO-",IsoCellCountry))]
  }
  y              <- x[IsoCellCountry,,]
  
  total          <- dimSums(dimSums(x, dim=c(2,3)), dim=1)
  returned       <- dimSums(dimSums(y, dim=c(2,3)), dim=1)
  lost           <- 1-(returned/total)
  
  if(lost!=0) cat(lost, " of the summed up values of the data set gets lost.\n")
  
  return(y)
}
