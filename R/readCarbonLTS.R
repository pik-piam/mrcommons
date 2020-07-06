#' readCarbonLTS
#' 
#' Read-in an Long term carbon storage data for historical period. 
#' 
#' 
#' @param subtype data subtype. 
#' @return magpie object of the FRA 2015 data
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("CarbonLTS","Lauk_et_al")
#' }
#' 
#' @importFrom magclass as.magpie getYears<- getNames<-
#' @importFrom madrat toolSubtypeSelect
#' @importFrom readxl read_xlsx
#' @importFrom zoo na.locf

readCarbonLTS <- function(subtype){
  
  if(subtype == "Lauk_et_al"){
    ## Source file name
    location <- "erl431725data.xlsx"
    
    ## Read file
    x <- read_xlsx(path = location,sheet = "figure1c",skip = 6)
    
    ## General cleanup
    x <- x[-1,-2] # Remove empty row and unit column
    x <- x[1,] # Only need wood data
    colnames(x)[1] <- "category"
    
    ## Convert to magpie object and convert to GtC
    x <- as.magpie(x) ## Convert 
    x <- x/1e3 ## Division by 10e3 to convert from ktC to MtC
    x <- x*44/12 ## Multiplication by 44/12 to convert from MtC to MtCO2e/yr
    
    getNames(x) <- "Emission"
    x <- add_dimension(x, dim=3.1, add="type", nm = "Annual (MtCO2/yr)")
    out <- x

  } else if (subtype == "Johnston_Radeloff") {
    ## Source file name
    location <- "pnas.1904231116.sd01.xlsm"
    
    cumulative <- read_xlsx(path = location,sheet = "Figures",skip = 50,n_max = 6)[,-2]
    colnames(cumulative) <- paste0("y",colnames(cumulative))
    colnames(cumulative)[1] <- "Emission"
    
    annual <- read_xlsx(path = location,sheet = "Figures",skip = 58,n_max = 6)[,-2]
    colnames(annual) <- paste0("y",colnames(annual))
    colnames(annual)[1] <- "Emission"
    
    ## Filling Historical SSP values
    cumulative <- na.locf(cumulative,na.rm = FALSE)
    annual     <- na.locf(annual,    na.rm = FALSE)
    
    cumulative_mo <- as.magpie(cumulative)
    cumulative_mo <- add_dimension(cumulative_mo, dim=3.1, add="type", nm = "Cumulative (GtCO2)")
    annual_mo <- as.magpie(annual)
    annual_mo <- add_dimension(annual_mo, dim=3.1, add="type", nm = "Annual (MtCO2/yr)")
    
    out <- mbind(annual_mo,cumulative_mo)
    
  } else stop("Cannot handle this subtype, select Lauk_et_al or Johnston_Radeloff.")
  
  return(out)
  
} 