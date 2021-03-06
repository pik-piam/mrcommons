#' @title readLPJmL
#' @description Read LPJmL content
#' @param subtype Switch between different input
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens, Abhijeet Mishra, Felicitas Beier
#' @seealso
#' \code{\link{readLPJ}}
#' @examples
#'
#' \dontrun{
#' readSource("LPJmL", subtype="LPJmL5:CRU4p02.soilc", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass readLPJ

readLPJmL <- function(subtype="LPJmL5:CRU4p02.soilc"){

  if(grepl("\\.",subtype)){
    
    subtype     <- strsplit(gsub(":", "/", subtype), split="\\.")
    folder      <- unlist(subtype)[1]
    subtype     <- unlist(subtype)[2]
    
  } else {stop("readLPJmL needs version and climatetype information")}
  
  files <- c(soilc              = "soilc_natveg.bin",
             soilc_layer        = "soilc_layer_natveg.bin",
             litc               = "litc_natveg.bin",
             vegc               = "vegc_natveg.bin",
             vegc_lpjcell       = "vegc_natveg.bin",
             alitfallc          = "alitfallc_natveg.bin",
             alitterfallc       = "alitterfallc_natveg.bin",
             alitfalln          = "alitfalln_natveg.bin",
             harvest            = "pft_harvest.pft.bin",
             irrig              = "cft_airrig.pft.bin",
             irrig_lpjcell      = "cft_airrig.pft.bin",
             cwater_b           = "cft_consump_water_b.pft.bin",
             cwater_b_lpjcell   = "cft_consump_water_b.pft.bin",           
             sdate              = "sdate.bin",
             hdate              = "hdate.bin",
             transpiration      = "mtransp_natveg.bin",
             discharge          = "mdischarge_natveg.bin",
             discharge_lpjcell  = "mdischarge_natveg.bin",
             runoff             = "mrunoff_natveg.bin",
             runoff_lpjcell     = "mrunoff_natveg.bin",
             evaporation        = "mevap_natveg.bin",
             evap_lake          = "mevap_lake.bin",
             evap_lake_lpjcell  = "mevap_lake.bin",
             mevap_lake         = "mevap_lake.bin",
             mevap_lake_lpjcell = "mevap_lake.bin",
             input_lake         = "input_lake.bin",
             input_lake_lpjcell = "input_lake.bin",
             mtranspiration     = "mtransp_natveg.bin",
             mdischarge         = "mdischarge_natveg.bin",
             mdischarge_lpjcell = "mdischarge_natveg.bin",
             mrunoff            = "mrunoff_natveg.bin",
             mrunoff_lpjcell    = "mrunoff_natveg.bin",
             mevaporation       = "mevap_natveg.bin",
             vegc_grass         = "mean_vegc_mangrass.bin",
             litc_grass         = "litc_mangrass.bin",
             soilc_grass        = "soilc_mangrass.bin"
  )

  file_name <- toolSubtypeSelect(subtype,files)

  if(tmp <- file.exists(file.path(folder,"tmp.out"))){
  
    tmp        <- readLines(file.path(folder,"tmp.out"))
    years      <- as.numeric(unlist(regmatches(tmp, gregexpr("\\d{4}", tmp))))
    start_year <- years[1]
    years      <- seq(years[1],years[2],1)
  
  } else {
  
    # default
    start_year  <- 1901
    years      <- seq(start_year,2017,1)
  }

  unit_transform <- 0.01               # Transformation factor gC/m^2 --> t/ha

  if (grepl("soilc|litc|vegc|alitfallc|alitterfallc|alitfalln|vegc_grass|litc_grass|soilc_grass", subtype) & subtype!="soilc_layer") {
    start_year  <- start_year           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 1                    # Number of bands in the .bin file
    avg_range   <- 1                    # Number of years used for averaging

    if (grepl("_lpjcell", subtype)){
      x <- readLPJ(
        file_name=file.path(folder,file_name),
        wyears=years,
        syear=start_year,
        averaging_range = avg_range,
        ncells=67420,
        bands=nbands,
        soilcells=FALSE)
    } else {
      x <- readLPJ(
        file_name=file.path(folder,file_name),
        wyears=years,
        syear=start_year,
        averaging_range=avg_range,
        bands=nbands,
        soilcells=TRUE)
    }
    
    # Transform to MAgPIE object
    if (grepl("_lpjcell", subtype)){
      class(x) <- "array"
      x <- collapseNames(as.magpie(x, spatial=1)) 
      lpj_cell_map <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",type="cell")
      getCells(x)  <- paste(lpj_cell_map$ISO,1:67420,sep=".")
      names(dimnames(x))[1] <- paste0(names(dimnames(x))[1],".region")
    } else {
      x <- collapseNames(as.magpie(x)) 
    }
    x <- x*unit_transform
    getNames(x) <- subtype

  } else if(grepl("*date*", subtype)){

    start_year  <- start_year           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 24                   # Number of bands in the .bin file
    avg_range   <- 1                    # Number of years used for averaging

    x <- readLPJ(
      file_name=file.path(folder,file_name),
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      datatype=integer(),
      bytes=2,
      soilcells = TRUE,
      ncells = 67420)

    x <- collapseNames(as.magpie(x))

  } else if(subtype%in%c("soilc_layer")){

    start_year  <- start_year           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 5                    # Number of bands in the .bin file
    avg_range   <- 1                    # Number of years used for averaging

    x <- readLPJ(
      file_name=file.path(folder,file_name),
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      soilcells=TRUE)

    x <- collapseNames(as.magpie(x))
    x <- x*unit_transform

    getNames(x)     <- paste0("soilc.",getNames(x))
    getSets(x)[4:5] <- c("data" ,"layer")

  } else if (grepl("transpiration|discharge|runoff|evaporation|evap_lake", subtype)) {

    start_year  <- start_year         # Start year of data set
    years       <- years              # Vector of years that should be exported
    nbands      <- 1                  # Number of bands in the .bin file
    avg_range   <- 1                  # Number of years used for averaging

    # monthly values
    if (grepl("_lpjcell", subtype)) {
      x <- readLPJ(
        file_name=file.path(folder,file_name),
        wyears=years,
        syear=start_year,
        averaging_range = avg_range,
        monthly=TRUE,
        ncells=67420,
        soilcells=FALSE)
    } else {
      x <- readLPJ(
        file_name=file.path(folder,file_name),
        wyears=years,
        syear=start_year,
        averaging_range = avg_range,
        monthly=TRUE,
        soilcells=TRUE)
    }

    # unit transformation
    if (grepl("transpiration", subtype)) { 
      # Transform units: liter/m^2 -> m^3/ha
      transp_unit_transform <- 10
      x <- x*transp_unit_transform
      
    } else if (grepl("discharge", subtype)) {
      # In LPJmL: (monthly) discharge given in hm3/d (= mio. m3/day)
      # Transform units of discharge: mio. m^3/day -> mio. m^3/month
      month_days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
      names(month_days) <- dimnames(x)[[3]]
      for(month in names(month_days)) {
        x[,,month,] <- x[,,month,]*month_days[month]
      }
      
    } else if (grepl("runoff|evap_lake", subtype)) {
    # In LPJmL: (monthly) runoff given in LPJmL: mm/month
      if (grepl("_lpjcell", subtype)){
        cb <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",type="cell")
        cell_area <- (111e3*0.5)*(111e3*0.5)*cos(cb$lat/180*pi)
        class(x) <- "array"
        x <- as.magpie(x, spatial=1)
        # Transform units: liter/m^2 -> liter
        x <- x*cell_area
      } else {
        # Get cellular coordinate information and calculate cell area
        cb <- as.data.frame(magpie_coord)
        cell_area  <- (111e3*0.5)*(111e3*0.5)*cos(cb$lat/180*pi)
        # Transform units: liter/m^2 -> liter
        x <- as.magpie(x)*cell_area
      }
      # Transform units: liter -> mio. m^3
      x <- x/(1000*1000000)

    } else if (grepl("evaporation", subtype)) { 
      # Transform units: liter/m^2 -> m^3/ha
      evap_unit_transform <- 10
      x <- x*evap_unit_transform
      
    }
    
    # Transform to MAgPIE object
    if (grepl("_lpjcell", subtype)){
      class(x) <- "array"
      x <- collapseNames(as.magpie(x, spatial=1)) 
      lpj_cell_map <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",type="cell")
      getCells(x)  <- paste(lpj_cell_map$ISO,1:67420,sep=".")
      names(dimnames(x))[1] <- paste0(names(dimnames(x))[1],".region")
    } else {
      x <- collapseNames(as.magpie(x)) 
    }
    
    if(grepl("layer", subtype)){
      subtype          <- gsub("_", "\\.", subtype)       # Expand dimension to layers
      getNames(x)      <- paste0(subtype,".",getNames(x))
      getSets(x)[4:6]  <- c("data" ,"layer","month")
    } else{
      getNames(x)      <- paste0(subtype,".",getNames(x))
      getSets(x)[4:5]  <- c("data" , "month")
    }
    
    # Annual value (total over all month)
    if(!grepl("^m", subtype)){
      x <- dimSums(x, dim="month")  
    }

  } else if(grepl("*harvest*", subtype)){

    start_year  <- start_year           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 32                   # Number of bands in the .bin file
    avg_range   <- 1                    # Number of years used for averaging

    x <- readLPJ(
      file_name=file.path(folder,file_name),
      wyears=years,
      syear=start_year,
      averaging_range=avg_range,
      bands=nbands,
      soilcells=TRUE)

    # Transformation factor gC/m^2 --> t/ha
    yield_transform <- 0.01/0.45
    x <- collapseNames(as.magpie(x))
    x <- x*yield_transform

  } else if(grepl("irrig|cwater_b", subtype)){ 
    
    start_year  <- start_year           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 32                   # Number of bands in the .bin file
    avg_range   <- 1                    # Number of years used for averaging
    
    if (grepl("_lpjcell", subtype)){
      x <- readLPJ(
        file_name=file.path(folder,file_name),
        wyears=years,
        syear=start_year,
        averaging_range = avg_range,
        bands=nbands,
        ncells=67420,
        soilcells=FALSE)
    } else {
      x <- readLPJ(
        file_name=file.path(folder,file_name),
        wyears=years,
        syear=start_year,
        averaging_range = avg_range,
        bands=nbands,
        soilcells=TRUE)
    }
    
    if (grepl("_lpjcell", subtype)){
      class(x) <- "array"
      x <- collapseNames(as.magpie(x, spatial=1))
      lpj_cell_map <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",type="cell")
      getCells(x) <- paste(lpj_cell_map$ISO,1:67420,sep=".")
      names(dimnames(x))[1] <- paste0(names(dimnames(x))[1],".region")
    } else {
      x <- collapseNames(as.magpie(x))
    }
    # Transform units (transform from: mm per year = liter per m^2 transform to: m^3 per ha)
      # 1 000 liter   = 1 m^3
      # 10 000 m^2    = 1 ha
      # 1 liter/m^2   = 10 m^3/ha
      # -> mm/yr * 10 = m^3/ha
    irrig_transform  <- 10
    x[,,"irrigated"] <- x[,,"irrigated"]*irrig_transform # units are now: m^3 per ha per year
    
  } else if(grepl("input_lake", subtype)){
    
    start_year  <- start_year           # Start year of data set
    years       <- years                # Vector of years that should be exported
    nbands      <- 1                    # Number of bands in the .bin file
    avg_range   <- 1                    # Number of years used for averaging
    
    if (grepl("_lpjcell", subtype)){
      x <- readLPJ(
        file_name=file.path(folder,file_name),
        wyears=years,
        syear=start_year,
        averaging_range = avg_range,
        bands=nbands,
        ncells=67420,
        soilcells=FALSE)
    } else {
      x <- readLPJ(
        file_name=file.path(folder,file_name),
        wyears=years,
        syear=start_year,
        averaging_range = avg_range,
        bands=nbands,
        soilcells=TRUE)
    }
    
    if (grepl("_lpjcell", subtype)){
      class(x) <- "array"
      x <- collapseNames(as.magpie(x, spatial=1))
      lpj_cell_map <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",type="cell")
      getCells(x) <- paste(lpj_cell_map$ISO,1:67420,sep=".")
      names(dimnames(x))[1] <- paste0(names(dimnames(x))[1],".region")
    } else {
      x <- collapseNames(as.magpie(x))
    }
    getNames(x) <- subtype
    
  } else {stop(paste0("subtype ",subtype," is not existing"))}

  return(x)

}
