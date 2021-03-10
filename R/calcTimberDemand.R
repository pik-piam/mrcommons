#' @title calcTimberDemand
#' @description 
#' Calculates the demand of timber from FAO data (including intermediate products).
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{calcFAOmassbalance_pre}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("TimberDemand")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @export

calcTimberDemand <- function(){
  x <- readSource("FAO_online","ForestProdTrade")
  
  ## Remove distinction between coniferous and non-coniferous part
  x <- x[,,sort(getNames(x)[grep(pattern = "coniferous",x = getNames(x),invert = T)])]
  
  ## Remove strange numbers (FAO item codes?)
  getNames(x,dim = "ItemCodeItem") <- gsub(pattern = "^[0-9]+\\|",
                                           replacement = "",
                                           x = getNames(x,dim = "ItemCodeItem"))
  
  ## Remove monetary data, we are only interested in m3 or mio. ton data
  x <- x[,,grep(pattern = "kUS\\$", x = getNames(x,dim = "ElementShort"), invert = T, value = T)]
  x <- x[,,sort(getNames(x))]
  
  ## FAO separated "Particle board and OSB" into "Particle board" and "Particle board and OSB (1961-1994)"
  ## Why? Who knows
  ## Merging back into "Particle board and OSB"
  x[,,"Particle board and OSB (1961-1994)"] <- x[,,"Particle board and OSB (1961-1994)"] + x[,,"Particle board"]

  ## drop "Particle Board"
  x <- x[,,"Particle board",invert=TRUE]
  
  ## Rename "Particle board and OSB (1961-1994)" to "Particle board and OSB"
  getNames(x,dim="ItemCodeItem") <- gsub(pattern = "Particle board and OSB \\(1961-1994\\)",replacement = "Particle board and OSB",x = getNames(x,dim="ItemCodeItem"))
  
  ## Extract variables we need and only choose variables which which have data for all three categories:
  ## Production in m3
  ## Import in m3
  ## Export in m3
  ## Here, wood pulp is an exception which is in mio. tonnes so we will assume 450 kg/m3 density there
  
  variables_needed <- c("Roundwood",
                        "Industrial roundwood","Wood fuel",
                        "Other industrial roundwood","Pulpwood, round and split, all species (production)",
                        "Sawlogs and veneer logs","Fibreboard","Particle board and OSB",
                        "Wood pulp","Sawnwood","Plywood","Veneer sheets","Wood-based panels")
  
  if(length(variables_needed[!(variables_needed %in% getNames(x,dim=1))])>0) message("Category '",variables_needed[!(variables_needed %in% getNames(x,dim=1))],"' does not exist in source FAO data. Manual correction to code will be needed.")
  ## in above variables, remeber that Sawlogs and veneer logs need an additional category called "SLVL based wood" which we will calculate back based on the sum of Sawnwood, plywood, venner sheets, wood residues and wood chips. 
  ## Also remeber that Pulpwood round and split only has data for production and not import and export so that has to be calculated back based on its constituents. 
  ## See https://i.imgur.com/VackFiv.png for better clarity.
  
  timber_fao <- x[,,variables_needed] ### Remember that wood pulp is in mio tonnes so we have to convert it to m3
  
  ## We'll consider the production, import and export separately
  
  production <- timber_fao[,,grep(pattern = "production",x = getNames(timber_fao,dim = "ElementShort"),value = TRUE)]
  import     <- timber_fao[,,grep(pattern = "import"    ,x = getNames(timber_fao,dim = "ElementShort"),value = TRUE)]
  export     <- timber_fao[,,grep(pattern = "export"    ,x = getNames(timber_fao,dim = "ElementShort"),value = TRUE)]
  
  ## Now we get rid of big name string of pulpwood
  getNames(production) <- gsub(x = getNames(production),pattern = "Pulpwood, round and split, all species \\(production)",replacement = "Pulpwood")
  
  ## Next step is to convert wood pulp from mio t to m3
  
  production[,,"Wood pulp"] <- production[,,"Wood pulp"] * 1000000 * 1000 / 450  ## 10^6 * 10^3 for mio t to kg. 450 for kg to m3
  import[,,"Wood pulp"] <- import[,,"Wood pulp"] * 1000000 * 1000 / 450  ## 10^6 * 10^3 for mio t to kg. 450 for kg to m3
  export[,,"Wood pulp"] <- export[,,"Wood pulp"] * 1000000 * 1000 / 450  ## 10^6 * 10^3 for mio t to kg. 450 for kg to m3
  
  ## Remove all units as now everything is converted to m3
  getNames(production,dim="ElementShort") <- gsub(pattern = "_m3",replacement = "",x = getNames(production,dim="ElementShort"))
  getNames(import,dim="ElementShort")     <- gsub(pattern = "_m3",replacement = "",x = getNames(import,dim="ElementShort"))
  getNames(export,dim="ElementShort")     <- gsub(pattern = "_m3",replacement = "",x = getNames(export,dim="ElementShort"))
  ## in the import and export objects, we need to add segment for pulpwood which is the sum of fibreboard, particle board OSB and Wood pulp
  
  to_add <- setdiff(getNames(production,dim = 1),getNames(export,dim = 1))
  
  import <- add_columns(x = import,addnm = to_add,dim = 3.1)
  export <- add_columns(x = export,addnm = to_add,dim = 3.1)
  
  import[,,to_add] <- 0 ## No import and export data for pulpwood from FAO
  export[,,to_add] <- 0 ## No import and export data for pulpwood from FAO
  
  ## Another category to add here is "Other Sawnwood" (refer to ??)
  to_add <- "Other sawnwood"
  production <- add_columns(x = production,addnm = to_add,dim = 3.1)
  production[,,to_add] <- production[,,"Sawlogs and veneer logs"] - dimSums(production[,,c("Plywood","Veneer sheets","Sawnwood")],dim = 3)
  if(min(production[,,to_add])<0) message("Negative values detected when adding category '",to_add,"'. Setting negative values to 0.")
  production[,,to_add][production[,,to_add]<0] = 0
  import <- add_columns(x = import,addnm = to_add,dim = 3.1)
  export <- add_columns(x = export,addnm = to_add,dim = 3.1)
  import[,,to_add] <- 0 ## No import and export data for Other sawnwood from FAO
  export[,,to_add] <- 0 ## No import and export data for Other sawnwood from FAO
  
  ###### Correction Stage
  ###### Refer to http://dx.doi.org/10.1016/j.rser.2016.09.107
  ###### Paper titled "The wood from the trees: The use of timber in construction"
  indicator_list <- c()
  for (indicator in getNames(production,dim = "ItemCodeItem")) {
    incorrect_data <- where(production[,,indicator][,,"production"] < export[,,indicator][,,"export"])$true
    if(length(unique(incorrect_data$regions))>0) indicator_list <- c(indicator_list,indicator)
    export[,,indicator][,,"export"] <- production[,,indicator][,,"production"]
  }
  message("Higher exports than production level detected in some countries. Setting these export values to production value. Following categories affected: ",paste0(indicator_list,", "))
  
  ## Now we are ready to merge back production, import and export
  timber_fao_cleaned <- mbind(production,import,export)
  
  ###### Data cleaning stage
  timber_fao_cleaned <- add_columns(timber_fao_cleaned,addnm = "other_util",dim = 3.2)
  timber_fao_cleaned[,,"other_util"] <- timber_fao_cleaned[,,"production"] + timber_fao_cleaned[,,"import"] - timber_fao_cleaned[,,"export"]
  
  timber_fao_cleaned <- add_columns(timber_fao_cleaned,addnm = "domestic_supply",dim = 3.2)
  timber_fao_cleaned[,,"domestic_supply"] <- timber_fao_cleaned[,,"other_util"]
  
  ## Now convert data to mio. m3
  timber_fao_cleaned <- timber_fao_cleaned / 1e6
  
  ## Check if negatvive values still exist
  if(min(timber_fao_cleaned)<0) {
    message("CAUTION: Negative values still exist in cleaned FAO timber production-import-export data.")
    timber_fao_cleaned[timber_fao_cleaned<0] = 0
    message("Such values are set to 0 now but could be problematic. Check R Script which processes this data")
    }
  
  out <- timber_fao_cleaned
  
  return(list(x=out,
              weight=NULL,
              min=0,
              unit="mio m3",
              description="Calculates the timber demand based on historical FAO data"))
  
  }
  