#' @title readISIMIP
#' @description Reads in ISIMIP data
#' @param subtype Type of ISIMIP data that should be read. 
#' It consists of variable ("airrww"), 
#' model ("cwatm","h08","lpjml","matsiro","mpi-hm","pcr-globwb"),
#' GCM ("ipsl-cm5a-lr","gfdl-esm2m","miroc5","hadgem2-es")  
#' and database version ("2a","2b","3a","3b"), separated by ":"
#' (e.g. "airww:LPJmL:gfdl-esm2m:2b")
#' @return MAgPIE object with the requested data
#' @author Jan Philipp Dietrich, Felicitas Beier, David Chen
#' @note Values for years before 1961 will be ignored to reduce overall object size
#' @examples
#' \dontrun{
#'  readSource("ISIMIP", convert=TRUE)
#' }
#'
#' @import madrat
#' @importFrom magclass getCoords
#' @importFrom raster brick subset
#' @importFrom abind abind

readISIMIP <- function(subtype="airww:LPJmL:gfdl-esm2m:2b"){

  .timevector <- function(start,end) {
    years      <- paste0("y",c(start:end))
    months    <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
    return(paste0(rep(years,each=12),".",months))
  }

  file <- Sys.glob("*.nc4")
  if(length(file)!=1) stop("Not able to identify input file!")
  years <- tail(strsplit(sub("\\..*$","",file),split="_")[[1]],2)
  
  r        <- brick(file)
  names(r) <- .timevector(years[1],years[2])
  r        <- subset(r,.timevector(max(years[1],1961),years[2]))

  x <- as.magpie(r, temporal=1)
  getSets(x,fulldim = FALSE)[2] <- "year.month"  
  
  return(x)
}
