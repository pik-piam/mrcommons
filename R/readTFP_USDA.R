#' @title readTFP_USDA
#' @description Reads the input shares from USDA's Agricultural total factor productivity growth indices assessment.
#' 
#'
#'
#'
#'
#' @return magpie object with fractions of different input factors in the overall production value
#' @author Edna J. Molina Bacca
#' @importFrom readxl read_excel
#' @importFrom stats reshape
#' @seealso [readSource()]
#' @examples
#'
#' \dontrun{
#' a <- readSource("TFP_USD")

#' }
#'
readTFP_USDA <- function() {

 #File
  file<-"AgTFPindividualcountries.xlsx"

  #Reads countries from the file
  countries<-read_excel(file, sheet = "Factor Shares",range = "D3:D190")
  FAON_countries<-read_excel(file, sheet = "Factor Shares",range = "B3:B190")
  regions <- cbind(countries,FAON_countries)

  #Available shares and their location in the file
  names<-c("revenue","AG_Labour","AG_Land","Livestock","Machinery","Materials_Crops","Materials_Animals")
  ranges<-c("K3:P190","R3:W190","Y3:AD190","AF3:AK190","AM3:AR190","AT3:AY190","BA3:BF190")

  #Function to extract the values of the shares and organize the data in a format easy to convert to a magpie object
   extract_fractions<-function(names=names,ranges=ranges,file=file,regions=regions){
     data<-NULL
     years_s<-as.character(seq(from = 1960, to = 2010, by = 10))
     
     for (n in 1:length(names)){
       data_int<-read_excel(file, sheet = "Factor Shares",range = ranges[n])
       colnames(data_int)<-years_s
       data_int<-cbind(regions,data_int)
       data_int$Input<-names[n]
       data<-rbind(data,data_int)
     }
     data <- reshape(data, varying = years_s, direction = "long",idvar = c("Country/territory","FAO N","Input"),v.names = "Value",timevar = "Year",times = years_s)
     rownames(data)<-1:length(rownames(data))
     colnames(data)<-c("Country","CountryCode","Input","Year","Value")
     return(data)
     }
  
  #Extracts data
  data <- extract_fractions(names,ranges,file,regions)

  #Reads fao countries
  isocode_FAO<-toolGetMapping("FAOiso_faocode.csv", where="mrcommons")
  
  #Merges read data with the fao mapping by country code
  data <- merge(data,isocode_FAO,by = "CountryCode",all=FALSE)[,c("ISO3","Year","Input","Value")]
  
  data[data$ISO3=="XCN","ISO3"]<-"CHN"
  
  #Creates magpie object (184 countries)
  x<-magpiesort(as.magpie(data,spatial=1, temporal=2, datacol=4))
  
  #Fills with zeros the countries that were not reported
  x<-toolCountryFill(x, fill = 0)


  return(x)
}

