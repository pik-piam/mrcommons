#' @title calcAgCapLabourShare
#' 
#' @description This function calculates historical capital shares (Capital + Labour) of the factor requirements using USDA  
#' 
#' @return MAgPIE object
#' @author Edna J. Molina Bacca
#' @seealso [calcAgCapLabourShare()]
#' 
#' @examples
#' \dontrun{ 
#' calcOutput("calcAgCapLabourShare")
#' }
#' 
calcAgCapLabourShare <- function() {
  
  #Reads requirements shares for capital and labour based on USDA
  Fraction<-collapseNames(calcOutput("FractionInputsUSDA",aggregate = FALSE))[,,c("Capital","Labor")]
  
  #Determines fraction between capital and labour
  Fraction_capital<-Fraction[,,"Capital"]/(dimSums(Fraction,dim=3.1))
  
  #In case of values different to finite makes them 0
  Fraction_capital[!is.finite(Fraction_capital)]<-0
  
  weight<-dimSums(collapseDim(calcOutput("Production",aggregate = FALSE)[,,"dm"]),dim=3.1)
  
  #Give 0 weigh to countries with unexpectedly high capital shares
  weight[c("BLZ","CRI","DOM","HND","JAM","MEX","NIC","PAN","SLV"),,]<-0

  years<-intersect(getYears(weight),getYears(Fraction_capital))
  
  weight<-weight[,years,]
  weight[Fraction_capital[,years,]==0]<-0
  x<-setNames(Fraction_capital[,years,],NULL)

  return(list(x=x,
              weight=weight,
              unit="Fraction capital", 
              description="Share of capital of the factor requirements"))
}
