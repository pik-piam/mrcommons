#' @title readISIMIP
#' @description Reads in ISIMIP data
#' @param subtype Type of ISIMIP data that should be read.
#' It consists of variable ("airrww"),
#' model ("cwatm","h08","lpjml","matsiro","mpi-hm","pcr-globwb"),
#' GCM ("ipsl-cm5a-lr","gfdl-esm2m","miroc5","hadgem2-es")
#' and database version ("2a","2b","3a","3b"), separated by ":"
#' (e.g. "airww:LPJmL:gfdl-esm2m:2b")
#' #' Similaryly for ISIMIP GGCMI phase3b data,  with scenarios and CO2 fert setting, downloads for all crops and irrigation settings
#' models ("LPJmL", "EPIC-IIASA", "pDSSAT", "CYGMA1p74"),
#' gcms ("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll"),
#' scenarios  ("historical", "ssp126", "ssp370", "ssp585"),
#' co2 ("default", "2015co2"),
#' version c("2a","2b","3a","3b")))
#' Example of yield subtype : "yields:EPIC-IIASA:ukesm1-0-ll:ssp585:default:3b"
#' @return MAgPIE object with the requested data
#' @author Jan Philipp Dietrich, Felicitas Beier, David Chen
#' @note Values for years before 1961 will be ignored to reduce overall object size
#' @examples
#' \dontrun{
#'  readSource("ISIMIP", convert=TRUE)
#' }
#' 
#' @importFrom magclass getCoords
#' @importFrom raster brick subset stack

readISIMIP <- function(subtype="airww:LPJmL:gfdl-esm2m:2b"){

  if  (grepl("airww",subtype)) {

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
  }


  if (grepl("yield", subtype)){
    files <- Sys.glob("*.nc")
    r <- stack(files)

    names(r) <- sub("^(.*)\\.(.*)$","\\2..\\1",names(r))
    names(r) <- gsub("X", "y", names(r))
    names(r) <- gsub("yield\\.", "", names(r))
    names(r) <- gsub("(\\.)(irr)", "\\2", names(r))
    names(r) <- gsub("(\\.)(wheat)", "\\2", names(r))
    names(r) <- gsub("(_wheat)", "wheat", names(r))
    names(r) <- gsub("rice1", "riceA", names(r), ignore.case=TRUE)
    names(r) <- gsub("rice2", "riceB", names(r), ignore.case=TRUE)


    #subset to year 1961 (1849+112) for faster processing
    if (grepl("historical", subtype)){
      r <- subset(r, which(as.numeric(gsub("\\D+","", names(r))) > 111))
      offset <- 1849
    } else { offset <- 2014 }

    x <- as.magpie(r)
    getNames(x) <- tolower(getNames(x))
    getYears(x) <- getYears(x, as.integer=TRUE) + offset

#fill missing cells with 0
  map <- toolGetMappingCoord2Country()
  missing_cells <- setdiff(map$coords, getItems(x, dim=1))
  fill <- new.magpie(cells_and_regions = missing_cells, years=getYears(x), names=getNames(x),  fill=0)
  x <- mbind(x, fill)
  }

  return(x)
}
