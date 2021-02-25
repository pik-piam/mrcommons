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
    
    if(subtype%in%c("transpiration|discharge|runoff|evaporation|evap_lake")){
      subtype_in <- paste0("m",subtype) } else { subtype_in <- subtype}
    
    readin_name <- paste0(version,":",climatetype,":",subtype_in)  
    
    ########## PLUG HIST + FUTURE ##########
   
    if(!grepl("historical", climatetype)){
      
      #For climate scenarios historical data has to be read in from a different file
      readin_hist <- toolSplitSubtype(readin_name, list(version=NULL, climatemodel=NULL, scenario=NULL, variable=NULL))
      readin_hist <- paste(gsub(readin_hist$scenario,"historical",readin_hist), collapse = ":")
      
      x     <- mbind(readSource("LPJmL", subtype=readin_hist, convert="onlycorrect"),
                     readSource("LPJmL", subtype=readin_name, convert="onlycorrect"))
      
      years <- getYears(x, as.integer = TRUE)
      x     <- x[,years[years >= 1980],]

    } else {
      
       x     <- readSource("LPJmL", subtype=readin_name, convert="onlycorrect")
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
      
    } else if(grepl("*date*", subtype)){
      
      unit <- "day of the year"
      
    } else if (grepl("transpiration|discharge|runoff|evaporation|evap_lake", subtype)) {
      
      # unit transformation
      if (grepl("transpiration|evaporation", subtype)) { 
        # Transform units: liter/m^2 -> m^3/ha
        unit_transform <- 10
        x <- x * unit_transform
        
      } else if (grepl("discharge", subtype)) {
        # In LPJmL: (monthly) discharge given in hm3/d (= mio. m3/day)
        # Transform units of discharge: mio. m^3/day -> mio. m^3/month
        dayofmonths <- as.magpie(c(jan=31,feb=28,mar=31,apr=30,may=31,jun=30,jul=31,aug=31,sep=30,oct=31,nov=30,dec=31))
        x           <- x * dayofmonths 
        
      } else if (grepl("runoff|evap_lake", subtype)) {
        # In LPJmL: (monthly) runoff given in LPJmL: mm/month
        landarea <- dimSums(calcOutput("LUH2v2", landuse_types="magpie", aggregate=FALSE, cellular=TRUE, cells="lpjcell", irrigation=FALSE, years="y1995"), dim=3)

        # Transform units: liter/m^2 -> liter
        x <- x * landarea
        
        # Transform units: liter -> mio. m^3
        x <- x / (1000*1000000)
        
      } 
      
      # Annual value (total over all month)
      if(!grepl("^m", subtype)){
        x <- dimSums(x, dim=3.2)  
      }
      
      units <- c(transpiration      = "m^3/ha",
                 discharge          = "mio. m^3",
                 runoff             = "mio. m^3",
                 evaporation        = "m^3/ha",
                 evap_lake          = "mio. m^3",
                 mevap_lake         = "mio. m^3",
                 mtranspiration     = "m^3/ha",
                 mdischarge         = "mio. m^3", 
                 mrunoff            = "mio. m^3", 
                 mevaporation       = "m^3/ha")
      
      unit <- toolSubtypeSelect(subtype,units)
      
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
    }
    
  } else if(stage %in% c("harmonized","harmonized2020")){
    
    #read in historical data for subtype
    x           <- calcOutput("LPJmL_new", version=version, climatetype=climatetype, subtype=subtype, subdata=subdata, stage="smoothed", aggregate=FALSE)
    
    if(climatetype == baseline_hist){
      
      stop("You can not harmonize the historical baseline.")
      
    } else if(stage=="harmonized"){
      
      Baseline    <- calcOutput("LPJmL_new", version=version, climatetype=baseline_hist,     subtype=subtype, subdata=subdata, stage="smoothed", aggregate=FALSE)
      out         <- toolHarmonize2Baseline(x, Baseline, ref_year=ref_year_hist)
      
    } else if((stage=="harmonized2020") & (climatetype != baseline_gcm)){
      
      Baseline2020    <- calcOutput("LPJmL_new", version=version, climatetype=baseline_gcm, subtype=subtype, subdata=subdata, stage="harmonized", aggregate=FALSE)
      out <- toolHarmonize2Baseline(x, Baseline2020, ref_year=ref_year_gcm)
      
    } else if((stage=="harmonized2020") & (climatetype == baseline_gcm)){
      # no need for harmonization
      out <- x
    
    }
    
  } else { stop("Stage argument not supported!") }
  
  
  return(list(
    x=out,
    weight=NULL,
    unit=unit, 
    description=paste0("Carbon output from LPJmL (",subtype,") for ", version, " and ", climatetype, " at stage: ", stage, "."),
    isocountries=FALSE))
}
