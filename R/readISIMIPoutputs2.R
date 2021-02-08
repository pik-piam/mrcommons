#' @title readISIMIPoutputs2
#' @description Reads in agricultural water withdrawals from ISIMIP2b output data, and GGCMI potential global crop yields from ISIMIP3b
#' @param subtype Data source to be read from and subtype separated by ".";
#' For water: subtype consisting of variable ("airrww") watermodel ("cwatm","h08","lpjml","matsiro","mpi-hm","pcr-globwb") and GCM ("ipsl-cm5a-lr","gfdl-esm2m","miroc5","hadgem2-es") separated by "_"
#' For yields: subtype is "ISIMIP3b:yields.cropmodel_gcm_ssp_co2" cropmodels ("LPJmL", "EPIC-IIASA") gcms ("ukesm1-0-ll","gfdl-esm4","ipsl-cm6a-lr") ssp ("ssp126", "ssp585")
#' Yields subtype example: "ISIMIP3b:yields.EPIC-IIASA_ukesm1-0-ll_ssp585_default"
#' @return MAgPIE object of non-agricultural water demand at 0.5 cellular level in mio. m^3
#' @author Felicitas Beier, David Chen, Jan Philipp Dietrich
#'
#' @examples
#' \dontrun{
#'  readSource("ISIMIPoutputs2", convert=TRUE)
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom ncdf4 nc_open ncvar_get ncatt_get nc_close
#' @importFrom raster brick subset as.matrix t
#' @importFrom abind abind

readISIMIPoutputs2 <- function(subtype="ISIMIP2b:water.histsoc_airrww_pcr-globwb_gfdl-esm2m"){
  if(grepl("\\.",subtype)){
    subtype     <- strsplit(gsub(":", "/", subtype), split="\\.")
    folder      <- unlist(subtype)[1]
    subtype     <- unlist(subtype)[2]

  }

  if (grepl("water",folder)) {
    # Construct path
    model    <- strsplit(subtype,split="\\_")
    time <- unlist(model)[1]
    variable <- unlist(model)[2]
    gcm      <- unlist(model)[4]
    model    <- unlist(model)[3]
    nc_file  <- paste0(folder,"/",time,"/",model,"_",gcm,"_ewembi_historical_histsoc_co2_",variable,"_global_monthly_1861_2005.nc4")

    r <- brick(nc_file)
    years      <- paste0("y",c(1861:2005))
    ysubset    <- paste0("y",c(1955:2005))
    months    <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
    names(r) <- paste0(rep(years,each=12),"..",months)
    subset   <- paste0(rep(ysubset,each=12),"..",months)
    r <- subset(r,subset)

   x <- as.magpie(r)
    
    # unit transformation: from: kg m-2 s-1 = mm/second, to: mm/month
    # (Note: 1 day = 60*60*24 = 86400 seconds)
    dayofmonths    <- as.magpie(c(jan=31,feb=28,mar=31,apr=30,may=31,jun=30,jul=31,aug=31,sep=30,oct=31,nov=30,dec=31))
    x <- x*dayofmonths*86400
    # yearly data
    x <- dimSums(x, dim=3)
    # Get cellular coordinate information and calculate cell area
    cell_area  <- (111e3*0.5)*(111e3*0.5)*cos(getCoords(x)$y/180*pi)
    # liter/m^2 -> liter
    x <- x*cell_area
    # liter -> mio. m^3
    x <- x/(1000*1000000)
  } else stop("subtype currently not supported!")
return(x)
}
