#' @title calcFractionInputsUSDA
#' @description Calculates the factor factor shares for crop production from USDA'S Inputs shares. 
#'
#' 
#'
#' @return magpie object of the shares of the factor requirements in agriculture (capital, labor, revenue, materials, land).
#' @author Edna J. Molina Bacca
#' @importFrom dplyr  intersect
#' @importFrom magclass magpiesort  
#' @importFrom magclass time_interpolate
#' @importFrom magclass collapseDim  
#' @importFrom magclass collapseNames
#' 
#' 
#' @seealso \code{\link{calcOutput}}
#' @examples
#'
#' \dontrun{
#' a <- calcOutput("FractionInputsUSDA")

#' }
#'
calcFractionInputsUSDA <- function() {
    
    inputs<-c("revenue" ,"Machinery", "AG_Land", "AG_Labour", "Materials_Crops") # inputs to read from the TFP_USDA data set
    TFP_shares_raw<-readSource("TFP_USDA")[,,inputs] # shares of inputs from USDA TFP data set writen as average, in decades
    
    #assuming the same share in the middle of the decade
    TFP_shares <- magpiesort(time_interpolate(TFP_shares_raw,interpolated_year = c((getYears(TFP_shares_raw,as.integer = TRUE)+5)),
                                              extrapolation_type = "constant",integrate_interpolated_years = TRUE)) 
    
    #reads value of production
    VoP_All<-readSource("FAO_online","ValueOfProd")
    crop_prod_vop<- VoP_All[,,"2041|Crops (PIN).Gross_Production_Value_(constant_2014_2016_million_US$)_(USD)"]# mio.ton. VoP for crops
    livestock_prod_vop<-VoP_All[,,"2044|Livestock (PIN).Gross_Production_Value_(constant_2014_2016_million_US$)_(USD)"] #mio. ton. VoP for livestock
    
    #Share of value of production between livestock and crop production
    crop_share_production<-collapseNames(crop_prod_vop/(crop_prod_vop+livestock_prod_vop))
    crop_share_production[!is.finite(crop_share_production)]<-0
    
    livestock_shared_input<-c("revenue" ,"Machinery", "AG_Labour")# factors that convine livestock and crops production
    crop_only<-c("AG_Land","Materials_Crops")# inputs assumed to be dedicated specifically to crop production
    
    years<-intersect(getYears(TFP_shares),getYears(crop_share_production))
    
    #to normalize overall summation of considered inputs
    total_input_crop_share<-(crop_share_production[,years,]*
                               (dimSums(TFP_shares[,years,livestock_shared_input],dim=3))+dimSums(TFP_shares[,years,crop_only],dim=3))
    
    # calculated fraction of production for the different inputs
    #capital
    fraction_capital <- crop_share_production[,years,]*TFP_shares[,years,"Machinery"]/total_input_crop_share
    fraction_capital[!is.finite(fraction_capital)]<-0
    
    #labor
    fraction_labor <- crop_share_production[,years,]*TFP_shares[,years,"AG_Labour"]/total_input_crop_share
    fraction_labor[!is.finite(fraction_labor)]<-0
    
    #revenue
    fraction_revenue <- crop_share_production[,years,]*TFP_shares[,years,"revenue"]/total_input_crop_share
    fraction_revenue[!is.finite(fraction_revenue)]<-0
    
    #land
    fraction_land <- TFP_shares[,years,"AG_Land"]/total_input_crop_share
    fraction_land[!is.finite(fraction_land)]<-0
    
    #materials
    fraction_materials <- TFP_shares[,years,"Materials_Crops"]/total_input_crop_share
    fraction_materials[!is.finite(fraction_materials)]<-0
    
    
    x<-mbind(fraction_capital,fraction_labor,fraction_revenue,fraction_land,fraction_materials)
    getNames(x)<-c("Capital","Labor","Revenue","Land","Materials")
    
    weight<-x
    weight[,,]<-1
    weight[!is.finite(x)]<- 0
    weight[x == 0 ]<- 0
    
    units<-"fraction"
    
  
  return(list(x=x,
              weight=weight,
              mixed_aggregation=NULL,
              unit=units,
              description="Factor shares for crops from USDA TFP data"))
}
