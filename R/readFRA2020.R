#' Read FRA2020
#' 
#' Read-in an FRA data from 2020 (forest resource assessment). 
#' 
#' 
#' @param subtype data subtype. 
#' @return magpie object of the FRA 2020 data
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("FRA2020","growing_stock")
#' }
#' 
#' @importFrom magclass as.magpie
#' @importFrom madrat toolSubtypeSelect toolCountry2isocode
#' @import countrycode 
#' @importFrom stats complete.cases
#' @export

readFRA2020 <- function(subtype){
  
  if (subtype=="growing_stock") { ## from FAO in mio. m3
    
    ## Declare the file names which contain relevant data
    forest_data  <- "fra2020-growingStock_forest.csv" # Origin = C:/PIK/data_processing/inputdata/sources/FRA2020/
    natveg_data  <- "fra2020-growingStock_natveg.csv"
    planted_data <- "fra2020-growingStock_planted.csv"
    
    ## Manual function to cleanup the data. Takes in source file as input
    process_data <- function(x){
      
      # Read the csv source file, First row is info not needed by us
      data   <- read.csv(x,header = TRUE,skip = 1)

      # Manually rename first column
      colnames(data)[1]  <- "Country"
      
      # Cleanup additional name info
      data$Country <- gsub(pattern = " \\(French Part\\)| \\(Desk study\\)",replacement = "",x = data$Country)
      
      # Convert from country names to ISO codes
      data$Country <- suppressWarnings(toolCountry2isocode(data$Country,warn = T,
                                                           mapping = c("United Kingdom of Great Britain and Northern Ireland" = "GBR",
                                                                       "Venezuela (Bolivarian Republic of)" = "VEN",
                                                                       "Bolivia (Plurinational State of)" = "BOL",
                                                                    #   "CÃ´te d'Ivoire" = "CIV",
                                                                       "French Guyana"= "GUF")))
      
      # Cleanup rows with NA in country names - Rows with no matching ISO code will be dropped
      data <- data[!is.na(data$Country),]
      
      # If data is not reported in any of the years, set it to 0, 
      # if partial data is reported, set it to mean values of known data in that country
      yr_count <- ncol(data)
      for (i in 1:nrow(data)) {
        na_count <- as.numeric(apply(data[i,], 1, function(x) sum(is.na(x))))
        if(na_count == yr_count-1){
          cat("No data reported for",data[i,1],"\n")
          data[i,-1] <- 0
        } else if ( na_count > 0 & na_count < yr_count-1){
          cat("Partial or missing data reported for",data[i,1],"\n")
          data[i,is.na(data[i,])] <- mean(as.numeric(as.vector(data[i,-1])),na.rm = TRUE)
        }
      }
      cat("\nCountries with no growing stock data will be set to 0.\n")
      cat("Countries with partial or missing growing stock data will be set to mean value of reported data.\n")
      
      # Replace X in colnames with y to make sure as.magpie recognizes this column as temporal dimension later
      colnames(data) <- gsub(pattern = "X",replacement = "y",x = colnames(data))
      
      # Return the cleaned data frame
      return(data)
    }
    
    # Plug in data via mbin - which uses cleaning up of source data while conversion to magpie object happens before mbind
    out <- mbind(setNames(as.magpie(process_data(forest_data)),"Forest"),
                 setNames(as.magpie(process_data(natveg_data)),"Natural vegetation"),
                 setNames(as.magpie(process_data(planted_data)),"Plantations"))
    
    # Add additional dimension to clealy state which data will be returned (set to subtype here)
    out <- add_dimension(x = out,dim = 3.1,nm = subtype, add = "Indicator")
    
    return(out)

  } else if (subtype!="growing_stock") {
    stop("Invalid or unsupported subtype ",subtype,". Check function documentation for valid subtypes. Returning NULL")
  }  
} 