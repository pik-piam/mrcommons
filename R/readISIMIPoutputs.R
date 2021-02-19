#' @title readISIMIPoutputs
#' @description Reads in agricultural water withdrawals from ISIMIP2b output data, and GGCMI potential global crop yields from ISIMIP3b
#' @param subtype Data source to be read from and subtype separated by ".";
#' For water: subtype consisting of variable ("airrww") watermodel ("cwatm","h08","lpjml","matsiro","mpi-hm","pcr-globwb") and GCM ("ipsl-cm5a-lr","gfdl-esm2m","miroc5","hadgem2-es") separated by "_"
#' For yields: subtype is "ISIMIP3b:yields.cropmodel_gcm_ssp_co2" cropmodels ("LPJmL", "EPIC-IIASA") gcms ("ukesm1-0-ll","gfdl-esm4","ipsl-cm6a-lr") ssp ("ssp126", "ssp585")
#' Yields subtype example: "ISIMIP3b:yields.EPIC-IIASA_ukesm1-0-ll_ssp585_default"
#' @return MAgPIE object of non-agricultural water demand at 0.5 cellular level in mio. m^3
#' @author Felicitas Beier, David CHen
#'
#' @examples
#' \dontrun{
#'  readSource("ISIMIPoutputs", convert=TRUE)
#' }
#'
#' @importFrom madrat toolGetMapping calcOutput
#' @importFrom magclass new.magpie as.magpie mbind dimSums
#' @importFrom ncdf4 nc_open ncvar_get ncatt_get nc_close
#' @importFrom raster brick subset as.matrix t
#' @importFrom abind abind

readISIMIPoutputs <- function(subtype="ISIMIP2b:water.histsoc_airrww_pcr-globwb_gfdl-esm2m"){
  if(grepl("\\.",subtype)){
    subtype     <- strsplit(gsub(":", "/", subtype), split="\\.")
    folder      <- unlist(subtype)[1]
    subtype     <- unlist(subtype)[2]

  }

  if (grepl("water",folder)) {
    # Construct path
    model    <- strsplit(subtype,split="\\_")
    time     <- unlist(model)[1]
    variable <- unlist(model)[2]
    gcm      <- unlist(model)[4]
    model    <- unlist(model)[3]
    nc_file  <- paste0(folder,"/",time,"/",model,"_",gcm,"_ewembi_historical_histsoc_co2_",variable,"_global_monthly_1861_2005.nc4")

    # Read data (actual irrigation water withdrawal):
    raw_data <- nc_open(nc_file) # unit: kg m-2 s-1

    lon    <- ncvar_get(raw_data,"lon")
    lat    <- ncvar_get(raw_data,"lat")
    t      <- ncvar_get(raw_data,"time")
    naval  <- ncatt_get(raw_data, "airrww", "_FillValue")
    nc_close(raw_data)

    start     <- seq(1,length(t),by=12)
    no_month  <- 12
    no_years  <- length(t)/no_month

    years     <- paste0("y",c(1861:2005))
    months    <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
    mapping   <- toolGetMapping(type="cell",name="CountryToCellMapping.csv")
    cellNames <- mapping$celliso
    x         <- NULL

    for (y in (45:no_years)){ # for faster calculation: use only part of the years
      x1        <- NULL
      year      <- years[y]
      nc_months <- paste0("X",c(t[start[y]]:(t[start[y]]+no_month-1)))

      for (m in (1:no_month)) {
        # Get data and transform to matrix
        tmp <- brick(nc_file)
        tmp <- subset(tmp,nc_months[m])
        tmp <- t(as.matrix(tmp))

        # Create and fill magpie object
        mag <- array(NA,dim=c(59199,1,1),dimnames=list(cellNames,year,months[m]))
        for (j in 1:59199) {
          mag[j,,] <- tmp[which(magpie_coord[j,1]==lon),which(magpie_coord[j,2]==lat)]
        }
        mag <- as.magpie(mag,spatial=1,temporal=2)
        getSets(mag) <- c("iso.cell","year","month")

        x1 <- mbind(x1,mag)
      }
      x <- mbind(x, x1)
    }

    # unit transformation: from: kg m-2 s-1 = mm/second, to: mm/month
    # (Note: 1 day = 60*60*24 = 86400 seconds)
    x <- x*86400
    # magpie object with days per month with same dimension as x
    tmp                  <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    month_days           <- new.magpie(names=dimnames(x)[[3]])
    month_days[,,]       <- tmp
    month_day_magpie     <- as.magpie(x)
    month_day_magpie[,,] <- 1
    month_day_magpie     <- month_day_magpie * month_days
    x <- x * month_day_magpie
    # yearly data
    x <- dimSums(x, dim=3)

    # from mm/month to mio. m^2
    # (Note: 1 mm = 1 liter/m^3)
    # Get cellular coordinate information and calculate cell area
    landarea <- dimSums(calcOutput("LUH2v2", landuse_types="magpie", aggregate=FALSE, cellular=TRUE, cells="magpiecell", irrigation=FALSE, years="y1995"), dim=3)
    # liter/m^2 -> liter
    x        <- x * landarea
    # liter -> mio. m^3
    x        <- x / (1000*1000000)
  }

return(x)
  
}
