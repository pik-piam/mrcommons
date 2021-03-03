#' @title calcLPJmL_new
#' @description Handle LPJmL data and its time behaviour (smoothing and harmonizing to baseline)
#' 
#' @param version Switch between LPJmL versions
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param subtype Switch between different lpjml input as specified in readLPJmL
#' @param subdata Switch between data dimension subitems
#' @param stage Degree of processing: raw, smoothed, harmonized, harmonized2020
#' 
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens, Felicitas Beier
#' @seealso
#' \code{\link{readLPJmL}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LPJmL_new", version="LPJmL4", climatetype="CRU_4", subtype="soilc", aggregate=FALSE)
#' }

calcLPJmL_new <- function(version="LPJmL4", climatetype="CRU_4", subtype="soilc", subdata=NULL, stage="harmonized2020"){
  
  ##### CONFIG #####
  baseline_hist <- "GSWP3-W5E5:historical"
  ref_year_hist <- "y2010"
  baseline_gcm  <- "GFDL-ESM4:ssp370"
  ref_year_gcm  <- "y2020"
  ##### CONFIG #####
  
  if(stage%in%c("raw","smoothed")){
    
    if (subtype%in%c("discharge|runoff|lake_evap|input_lake")) {
      # calcLPJmL subtypes (returned by calcLPJmL) that are calculated based on different original LPJmL subtypes 
      readinmap <- c(lake_evap    = "mpet",  # mpet_natveg    lake_evap  = pet   * lake_shr * cell_area
                     input_lake   = "aprec", # aprec_natveg   input_lake = aprec * lake_shr * cell_area
                     discharge    = "mdischarge",
                     runoff       = "mrunoff")

       subtype_in <- toolSubtypeSelect(subtype, readinmap)
        
    } else { subtype_in <- subtype}
    
    readin_name <- paste0(version,":",climatetype,":",subtype_in)  
    
    ########## PLUG HIST + FUTURE ##########
   
    if(!grepl("historical", climatetype)){
      
      #For climate scenarios historical data has to be read in from a different file
      readin_hist <- toolSplitSubtype(readin_name, list(version=NULL, climatemodel=NULL, scenario=NULL, variable=NULL))
      readin_hist <- paste(gsub(readin_hist$scenario,"historical",readin_hist), collapse = ":")
      
      x     <- mbind(readSource("LPJmL_new", subtype=readin_hist, convert=FALSE),
                     readSource("LPJmL_new", subtype=readin_name, convert=FALSE))
      
      years <- getYears(x, as.integer = TRUE)
      x     <- x[,years[years >= 1980],]

    } else {
      
       x     <- readSource("LPJmL_new", subtype=readin_name, convert=FALSE)
       years <- getYears(x, as.integer = TRUE)
       x     <- x[,years[years >= 1930],]
    }
    ########## PLUG HIST + FUTURE ##########
    
    if(!is.null(subdata)){
      if(!all(subdata %in% getNames(x))) stop(paste0("Subdata items '", subdata,"' are not part of selected LPJmL subtype!"))
      x <- x[,,subdata]
    }
    
    ########## UNIT TRANSFORMATION ###############
    
    if (grepl("soilc|soilc_layer|litc|vegc|alitfallc|alitterfallc|vegc_grass|litc_grass|soilc_grass", subtype)) {
      
      unit_transform <- 0.01
      x <- x*unit_transform
      unit <- "tC/ha"
      
    } else if (grepl("*date*", subtype)) {
      
      unit <- "day of the year"
      
    } else if (grepl("aet|discharge|runoff|lake_evap|input_lake", subtype)) {
      
      # unit transformation
      if (grepl("aet", subtype)) { 
        # Annual evapotranspiration (evaporation + transpiration + interception) given in liter/m^2
        # Transform units: liter/m^2 -> m^3/ha
        unit_transform <- 10
        x              <- x * unit_transform
        
      } else if (grepl("discharge", subtype)) {
        # In LPJmL: (monthly) discharge given in hm3/d (= mio. m3/day)
        # Transform units of discharge: mio. m^3/day -> mio. m^3/month
        dayofmonths <- as.magpie(c(jan=31,feb=28,mar=31,apr=30,may=31,jun=30,jul=31,aug=31,sep=30,oct=31,nov=30,dec=31))
        x           <- x * dayofmonths 
        
        # Annual value (total over all month)
        if (!grepl("^m", subtype)) {
          x <- dimSums(x, dim=3.2)  
        }
        
      } else if (grepl("runoff", subtype)) {
        ## In LPJmL: (monthly) runoff given in LPJmL: mm/month
        # Transform units: liter/m^2 -> liter
        landarea <- collapseNames(dimSums(readSource("LUH2v2", subtype="states", convert="onlycorrect")[,"y1995",], dim=3))
        x        <- x * landarea
        # Transform units: liter -> mio. m^3
        x <- x / (1000*1000000)
        
        # Annual value (total over all month)
        if (!grepl("^m", subtype)) {
          x <- dimSums(x, dim=3.2)  
        }
        
      } else if (grepl("lake_evap|input_lake", subtype)) {
        ## In LPJmL: given in mm (=liter/m^2)
        # Multiply by lake share
        lake_share <- readSource("LPJmLInputs", subtype="lakeshare", convert="onlycorrect")
        x          <- x * lake_share
        # Transform units: liter/m^2 -> liter
        cb        <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type="cell")
        cell_area <- (111e3*0.5)*(111e3*0.5)*cos(cb$lat/180*pi)
        x         <- x * cell_area
        # Transform units: liter -> mio. m^3
        x <- x / (1000*1000000)
        
        # Annual value (total over all month)
        if (grepl ("lake_evap", subtype)) {
          x <- dimSums(x, dim=3.2)  
        }
      } 
      
      units <- c(aet                 = "m^3/ha",
                 discharge           = "mio. m^3",
                 runoff              = "mio. m^3",
                 evap_lake           = "mio. m^3",
                 mevap_lake          = "mio. m^3",
                 mevapotranspiration = "m^3/ha",
                 mdischarge          = "mio. m^3", 
                 mrunoff             = "mio. m^3")
      
      unit <- toolSubtypeSelect(subtype, units)
      
    } else if(grepl("*harvest*", subtype)){
      
      yield_transform <- 0.01/0.45
      x <- x*yield_transform
      unit <- "tDM/ha"
      
    } else if(grepl("irrig|cwater_b", subtype)){ 
      
      irrig_transform  <- 10
      x[,,"irrigated"] <- x[,,"irrigated"] * irrig_transform # units are now: m^3 per ha per year
      unit             <- "m^3/ha"
      
    } else if(grepl("input_lake", subtype)){
      
      unit <- "mio. m^3"
      
    } else {stop(paste0("subtype ",subtype," is not existing"))}
    
    ########## UNIT TRANSFORMATION ###############
    
    if(stage=="smoothed"){
      out <- toolSmooth(x)
    } else {
      out <- x
    }
    
  } else if(stage=="harmonized"){
    
    if(climatetype == baseline_hist) stop("You can not harmonize the historical baseline.")
    
    x           <- calcOutput("LPJmL_new", version=version, climatetype=climatetype, subtype=subtype, subdata=subdata, stage="smoothed", 
                              aggregate=FALSE, supplementary=TRUE)
    unit        <- x$unit
    x           <- x$x
    
    Baseline    <- calcOutput("LPJmL_new", version=version, climatetype=baseline_hist,     subtype=subtype, subdata=subdata, stage="smoothed", aggregate=FALSE)
    out         <- toolHarmonize2Baseline(x, Baseline, ref_year=ref_year_hist)
    
  } else if(stage=="harmonized2020"){
    
    #read in historical data for subtype
    Baseline2020    <- calcOutput("LPJmL_new", version=version, climatetype=baseline_gcm, subtype=subtype, subdata=subdata, stage="harmonized", 
                                  aggregate=FALSE, supplementary=TRUE)
   
    unit            <- Baseline2020$unit
    Baseline2020    <- Baseline2020$x
     
    if(climatetype == baseline_gcm){
      out <- Baseline2020
      
    } else {
      
      x   <- calcOutput("LPJmL_new", version=version, climatetype=climatetype, subtype=subtype, subdata=subdata, stage="smoothed", aggregate=FALSE)
      out <- toolHarmonize2Baseline(x, Baseline2020, ref_year=ref_year_gcm)
    }
    
  } else { stop("Stage argument not supported!") }
  
  
  return(list(
    x=out,
    weight=NULL,
    unit=unit, 
    description=paste0("Carbon output from LPJmL (",subtype,") for ", version, " and ", climatetype, " at stage: ", stage, "."),
    isocountries=FALSE))
}
