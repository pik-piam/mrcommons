#' Convert updated James data
#'
#' 
#' @param x MAgPIE object containing James data region resolution
#' @param subtype subtype of GDP indicator to be selected
#' @return GDP per capita in USD05 in PPP or MER as magpie object
#' @author David CHen 
#' @seealso \code{\link{readJames}}
#' @examples
#' 
#' \dontrun{ a <- convertSource("James2019","IHME_USD05_PPP_pc")
#' }
#' @importFrom countrycode countrycode

convertJames2019 <- function(x,subtype) {
  x<-x[c("USSR_FRMR","CHN_354","CHN_361"),,,invert=TRUE] #Macao and HKG and Former USSR have 0 values in the dataset
  x<-toolCountryFill(x[,,subtype],fill = 0) 
  
  #fill missing islands not in MissingIslands, using older James
  old <- readSource("James", subtype=subtype)
  missing <- time_interpolate(old[c("ABW","PYF","NCL"),,], interpolated_year = c(1950:2019))
  x[c("ABW","PYF","NCL"),,] <- missing
  
  # use old HKG and MAC shares, and subtract from CHN
  # new james has much higher (double in earliest time steps)
  # historical china values
  pop<-readSource("WDI",subtype = "SP.POP.TOTL")
  oldyears <- intersect(getYears(pop), getYears(old))
  old_t <- pop[,oldyears,]*old[,oldyears,]
  old_t <- collapseNames(old_t)
  shr <- old_t[c("HKG","MAC"),,]/dimSums(old_t[c("CHN","HKG","MAC"),,], dim=1)

  newyears <- intersect(getYears(pop), getYears(x))
  shr <- time_interpolate(shr, interpolated_year = newyears)
  
  
  x <- pop[,newyears,]*x[,newyears,]
  x[c("HKG","MAC"),,] <- shr*x["CHN",,]
  x["CHN",,] <- x["CHN",,]-dimSums(x[c("HKG","MAC"),,], dim=1)
  x <- x/pop
  x <- collapseNames(x, preservedim=4)
  
  return(x)
}  
