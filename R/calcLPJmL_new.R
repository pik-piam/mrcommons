#' @title calcLPJmL_new
#' @description Handle LPJmL data and its time behaviour (smoothing and harmonizing to baseline)
#' 
#' @param version Switch between LPJmL versions
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param subtype Switch between different lpjml input as specified in readLPJmL
#' @param subdata Switch between data dimension subitems
#' @param stage Degree of processing: raw, smoothed, harmonized
#' 
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens, Felicitas Beier
#' @seealso
#' \code{\link{readLPJmL}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LPJmL", version="LPJmL4", climatetype="CRU_4", subtype="soilc", aggregate=FALSE)
#' }

calcLPJmL_new <- function(version="LPJmL4", climatetype="CRU_4", subtype="soilc", subdata=NULL, stage="harmonized"){
  
  if(stage%in%c("raw","smoothed")){
    
    readin_name <- paste0(version,":",climatetype,".",subtype)  
    LPJmL_input <- readSource("LPJmL", subtype=readin_name, convert="onlycorrect")
    
    if(!is.null(subdata)){
      if(!all(subdata %in% getNames(LPJmL_input))) stop(paste0("Subdata items '", subdata,"' are not part of selected LPJmL subtype!"))
      LPJmL_input <- LPJmL_input[,,subdata]
    }
    
    if("y2099" %in% (y <- getYears(LPJmL_input))){
      LPJmL_input <- LPJmL_input[,tail(y, length(1980:2099)),] #crop GCM data to shorter time periods
    } else {
      LPJmL_input <- LPJmL_input[,1931:as.numeric(substring(tail(y,1),2)),] #crop CRU data to shorter time periods
    }
    
    if(stage=="smoothed") LPJmL_input <- toolSmooth(LPJmL_input)
    
  } else if(stage=="harmonized"){
    
    #read in historical data for subtype
    x           <- calcOutput("LPJmL", version=version, climatetype=climatetype, subtype=subtype, subdata=subdata, stage="smoothed", aggregate=FALSE)
    Baseline    <- calcOutput("LPJmL", version=version, climatetype="CRU_4",     subtype=subtype, subdata=subdata, stage="smoothed", aggregate=FALSE)
    #harmonize to baseline
    LPJmL_input <- toolHarmonize2Baseline(x, Baseline)
    
  } else { stop("Stage argument not supported!") }
  
  # unit table for subtypes from readLPJmL
  units <- c(soilc              = "tC/ha",
             soilc_layer        = "tC/ha",
             litc               = "tC/ha",
             vegc               = "tC/ha",
             alitfallc          = "tC/ha",
             alitfalln          = "tN/ha",
             harvest            = "tDM/ha",
             irrig              = "m^3/ha",
             irrig_lpjcell      = "m^3/ha",
             cwater_b           = "m^3/ha",
             cwater_b_lpjcell   = "m^3/ha",
             sdate              = "day of the year",
             hdate              = "day of the year",
             transpiration      = "m^3/ha",
             discharge          = "mio. m^3",
             discharge_lpjcell  = "mio. m^3",
             runoff             = "mio. m^3",
             runoff_lpjcell     = "mio. m^3",
             evaporation        = "m^3/ha",
             evap_lake          = "mio. m^3",
             evap_lake_lpjcell  = "mio. m^3",
             mevap_lake         = "mio. m^3",
             mevap_lake_lpjcell = "mio. m^3",
             input_lake         = "mio. m^3",
             input_lake_lpjcell = "mio. m^3",
             mtranspiration     = "m^3/ha",
             mdischarge         = "mio. m^3", 
             mdischarge_lpjcell = "mio. m^3", 
             mrunoff            = "mio. m^3", 
             mrunoff_lpjcell    = "mio. m^3", 
             mevaporation       = "m^3/ha",
             vegc_grass         = "tC/ha",
             litc_grass         = "tC/ha",
             soilc_grass        = "tC/ha"
  )
  
  unit <- toolSubtypeSelect(subtype,units)
  
  return(list(
    x=LPJmL_input,
    weight=NULL,
    unit=unit, 
    description=paste0("Carbon output from LPJmL (",subtype,") for ", version, " and ", climatetype, " at stage: ", stage, "."),
    isocountries=FALSE))
}
