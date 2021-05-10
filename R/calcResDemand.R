#' @title calcResDemand
#' @description Calculates the demand for Crop Residues
#'
#' @param cellular If TRUE calculation and output on cellular level
#' @param scenario define scenario switch for sensititvy analysis 
#'                 for historical SOC budget
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcResBiomass}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ResDemand")
#' }
#' @importFrom magclass setNames

calcResDemand<-function(cellular = FALSE, scenario="dafault"){
  
  mapping       <- toolGetMapping("mappingCrop2Residue.csv", where="mrcommons", type="sectoral")
  
  res_cereals    <- mapping$kcr[mapping$kres=="res_cereals"]
  res_fibrous    <- mapping$kcr[mapping$kres=="res_fibrous"]
  res_nonfibrous <- mapping$kcr[mapping$kres=="res_nonfibrous"]
  res            <- c(res_cereals,res_fibrous,res_nonfibrous)
  
  kres           <- findset("kres")
  past           <- findset("past")
  
  dev_state_past <- collapseNames(calcOutput("DevelopmentState",aggregate = F)[,past,"SSP2"])
  
  if(cellular){ 
    dev_state_past <- toolIso2CellCountries(dev_state_past)
  }
  
  biomass1       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular= cellular, plantparts="ag", aggregate = FALSE, scenario=scenario)[,,res_cereals]),dim=3.1), add = "kres", nm = "res_cereals")
  biomass2       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular= cellular, plantparts="ag", aggregate = FALSE, scenario=scenario)[,,res_fibrous]),dim=3.1), add = "kres", nm = "res_fibrous")
  biomass3       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular= cellular, plantparts="ag", aggregate = FALSE, scenario=scenario)[,,res_nonfibrous]),dim=3.1), add = "kres", nm = "res_nonfibrous")
  biomass        <- mbind(biomass1,biomass2,biomass3)
  
  #interpolate for yearly data, keeping ends constant to not have negatives
  
  iyears <- getYears(biomass)
  dev_state_past <- time_interpolate(dev_state_past, interpolated_year = iyears, integrate_interpolated_years = TRUE)
  dev_state_past[,c(1961:1964),] <- setYears(dev_state_past[,1965,],NULL)
  dev_state_past[,c(2011:getYears(dev_state_past, as.integer = TRUE)[length(getYears(dev_state_past))]),] <- setYears(dev_state_past[,2010,],NULL)
  
  
  material       <- mbind(biomass[,,"res_cereals"] * (dev_state_past*0 + ( 1 - dev_state_past ) * 0.05) , biomass[,,c("res_fibrous","res_nonfibrous")] * 0)
  bioenergy      <- biomass * (dev_state_past * 0 + ( 1 - dev_state_past ) * 0.1)
  
  feed           <- dimSums(calcOutput("FeedPast", balanceflow=FALSE, cellular= cellular, aggregate = F,nutrients="dm")[,,kres], dim=c(3.1,3.3))
  
  if(grepl("freeze*",scenario)){
    
    biomass1       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular= cellular, plantparts="ag", aggregate = FALSE, scenario="default")[,,res_cereals]),dim=3.1), add = "kres", nm = "res_cereals")
    biomass2       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular= cellular, plantparts="ag", aggregate = FALSE, scenario="default")[,,res_fibrous]),dim=3.1), add = "kres", nm = "res_fibrous")
    biomass3       <- add_dimension(dimSums(collapseNames(calcOutput("ResBiomass", cellular= cellular, plantparts="ag", aggregate = FALSE, scenario="default")[,,res_nonfibrous]),dim=3.1), add = "kres", nm = "res_nonfibrous")
    biomassOld     <- mbind(biomass1,biomass2,biomass3)
    
    feedshare <- toolConditionalReplace(feed/biomassOld[,,"dm"], "is.na()", 0)
    feedNew   <- collapseNames(feedshare * biomass[,,"dm"])
    feedNew[feedNew==0] <- feed[feedNew==0]
    feed      <- feedNew 
  }
  
  #attribute calculation for feed demand based on real residue mixture for each spatial unit 
  #kres attributes lead to distortion of phosphorus share within each kres
  attributes <- round(collapseNames(biomass/biomass[,,"dm"]),8)
  
  #NA means no residue production at all, so attributes can be put to zero
  #This will neglect residue feed demand in some areas, where it was wrongly assumend
  
  attributes[is.na(attributes)] <- 0 
  
  if(any((feed!=0)&(attributes==0))){
    vcat(2,"Feed demand was neglected in areas, where there was no residue biomass available at all.")
  }
  
  feed       <- attributes * feed
  
  production <- domestic_supply <- feed + material + bioenergy
  
  out        <- mbind( add_dimension(production,dim = 3.1,nm = "production"),
                       add_dimension(domestic_supply,dim = 3.1,nm ="domestic_supply"),
                       add_dimension(feed,dim = 3.1,nm ="feed"),
                       add_dimension(material,dim = 3.1,nm ="other_util"),
                       add_dimension(bioenergy,dim = 3.1,nm ="bioenergy"))
                    
  return(list(x=out,
              weight=NULL,
              unit="Mt DM, Nr, P, K, WM, Pj Energy",
              description="Crop Residues Harvest and use",
              isocountries =!cellular))
}
