#' @title readFishstatJ_FAO
#' @description  Reads data of fisheries generated using the FishstatJ app of FAO. Read-in specifically, exports_value, exports_quantity, and/or overall production of fish/aquatic products.
#'
#'
#' @param subtype data subtype needed. Either "exportsValue", "exportsQuantity", or "Production"
#' @return magpie object of either tonnes of liveweight or 1000 current USD
#' @author Edna J. Molina Bacca
#' @importFrom stats reshape
#' @seealso \code{\link{readSource}}
#' @examples
#'
#' \dontrun{
#' a <- readSource("FishstatJ_FAO","Production")
#' a <- readSource("FishstatJ_FAO","exportsQuantity")
#' a <- readSource("FishstatJ_FAO","exportsValue")
#' }

readFishstatJ_FAO <- function(subtype="Production") {

  #Files generated using the FishstatJ app
  files <- c(exportsValue       = "FAOSTAT_data_1-26-2021_FishesTradeUSD.csv",
             exportsQuantity    = "FAOSTAT_data_1-26-2021_FishesTradeTonns.csv",
             Production         = "FAOSTAT_data_1-26-2021_FishesProduction.csv" )
  
  #Subsetting based on type of requested output
  file <- toolSubtypeSelect(subtype,files)
  isocode_FAO<-toolGetMapping("FAOiso_faocode.csv", where="mrcommons")
  
  #Reads data 
  data <- read.csv(file=paste(path.package("mrcommons"),paste0("/extdata/sectoral/",file),sep="")) 
  
  #Function to clean-up the data 
  fao_cleaning <- function(data = data, mapping = isocode_FAO, subsetvar = "Unit..Name.", UnitVar= "Tonnes - live weight", Value = "Production"){

    years_stats     <- paste0("X.",1984:2018,".") #wide format
    data <- if(Value == "Production") data[,c("Country..Name.", "Unit..Name." ,years_stats)] else if(Value %in% c("exportsValue","exportsQuantity")) data[,c("Country..Name.", "Trade.flow..Name.", "Unit..Name.", years_stats)]#select needed columns
    years_stats     <- as.character(1984:2018)
    colnames(data) <- if(Value == "Production") c("Country", "Variable", years_stats) else if(Value %in% c("exportsValue","exportsQuantity")) c("Country","Variable","Unit", years_stats)
    data <- data[data$Variable == UnitVar,] #read only "Tonnes - live weight","Export"
    data <- reshape(data, varying = years_stats, direction = "long",idvar = c("Country","Variable"),v.names = "Value",timevar = "Year",times = years_stats) #from wide to long format
    rownames(data) <- 1:nrow(data) #fix names of rows
    data <- merge(data,mapping,by = "Country")
    data <- data[,c("ISO3","Year","Value")]
    data[,"Year"]<-as.numeric(data[,"Year"])
    x <- magpiesort(as.magpie(data,temporal = 2,spatial = 1,datacol = 3)) # converts to magpie object tonnes - live weight, current 1000 USD
    x <- toolCountryFill(x = x,fill = 0) # fill with zeros
    getNames(x) <- Value
    
    if(Value == "exportsValue"){
      
      for (i in getYears(x,as.integer = TRUE)){
        x[,i,] <- x[,i,]*(1+0.04)^(2020-i) 
      }
      
    } # brings values to present usd for exports value
    
    
    return(x)
  }


 #Cleaning based on output subtype selected
  if (subtype == "Production") {

  x <- fao_cleaning(data = data, mapping = isocode_FAO, subsetvar = "Unit..Name.", UnitVar = "Tonnes - live weight", Value = "Production")

  } else if (subtype == "exportsQuantity") {

    x <- fao_cleaning(data = data, mapping = isocode_FAO, subsetvar = "Trade.flow..Name.", UnitVar = "Export", Value = "exportsQuantity")

    } else if (subtype == "exportsValue") {

      x <- fao_cleaning(data = data, mapping = isocode_FAO, subsetvar = "Trade.flow..Name.", UnitVar = "Export", Value = "exportsValue")

    }

  return(x)
}

