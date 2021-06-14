#' @title toolLPJmLVersion
#'
#' @description Specify default settings for LPJmL version and baseline settings
#' 
#' @param version Switch between LPJmL versions (including add-ons (+*) for further version specification)
#' @param climatetype Switch between different climate scenarios
#' 
#' @return configuration as list
#' @author Kristine Karstens
#'
#' @importFrom stringr str_split
#' 
#' @export

toolLPJmLVersion <- function(version, climatetype){
  
  cfg <- NULL
  
  ##### DEFAULT CONFIG #####
  cfg$baseline_hist <- "GSWP3-W5E5:historical"
  cfg$ref_year_hist <- "y2010"
  cfg$baseline_gcm  <- "GFDL-ESM4:ssp370"
  cfg$ref_year_gcm  <- "y2020"
  ##### DEFAULT CONFIG #####
  
  ##### ADDON CONFIG #####
  # overwrite default settings and LPJmL version for add-on tag in version argument
  # implemented add-ons:
  # * `+oldGSWP3`              - use older LPJmL version for GSWP3 
  #                               as long as new GSWP3 is not available
  # * `+baseline_gcm<GCM:RCP>` - use another baseline for 2010--2020
  
  if(grepl("\\+", version)){
    
    tmp     <- unlist(str_split(version,"\\+"))
    
    for(i in tmp[-1]){
      
      if(        i == "oldGSWP3" & climatetype == "GSWP3-W5E5:historical"){
        
        if(grepl("LPJmL4",tmp[1])) cfg$lpjml_version <- "LPJmL4_for_MAgPIE_84a69edd"
        if(grepl("ggcmi", tmp[1])) cfg$lpjml_version <- "ggcmi_phase3_nchecks_fbed5c8b_newparam"
        
      } else if( grepl("baseline_gcm", i) ){
        
        cfg$baseline_gcm  <- gsub("baseline_gcm","",i) 
        
      } else {
        
        cfg$lpjml_version       <- tmp[1]
      }
    }
    
  } else { cfg$lpjml_version <- version } #use version as lpjml_version in absence of addon
  ##### ADDON CONFIG #####
  
  return(cfg)
}
