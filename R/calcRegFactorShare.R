#' @title calcRegFactorShare
#' 
#' @description This function calculates the regression parameters (a and b) for the function Share=a*log10(GDP)+b. Where share is the adjusted share between capital and labour.
#' 
#' 
#' @return MAgPIE object at global level with slope and intersect as items
#' @author Debbora Leip, Edna J. Molina Bacca
#' @seealso \code{\link{calcOutput}},\code{\link{calcFactorIntensity}} 
#' @param datasource Only USDA available
#' @param factor "lab" for Labour and "cap" for capital
#' @importFrom stats lm
#' 
#' @examples
#' \dontrun{
#' calcOutput("calcRegFactorShare")
#' }
#' 
#' 
calcRegFactorShare <- function(datasource="USDA",factor="cap") {
  
  if (datasource == "USDA") {
    # raw USDA cost shares
    USDA_shares <- readSource("TFP_USDA")[,, c("AG_Labour", "Machinery")]
    
    # assuming the same share in the middle of the decade
    USDA_shares <- magpiesort(time_interpolate(USDA_shares,
                                               interpolated_year = c((getYears(USDA_shares, as.integer = TRUE) + 5)),
                                               extrapolation_type = "constant", integrate_interpolated_years = TRUE))
    
    # Production (in terms of dry matter) as weight
    prod_crops <- dimSums(collapseDim(calcOutput("Production", products = "kcr", aggregate = FALSE)[, , "dm"]), dim = 3)
    prod_livst <- dimSums(collapseDim(calcOutput("Production", products = "kli", aggregate = FALSE)[, , "dm"]), dim = 3)
    total_prod <- prod_crops + prod_livst
    weight <- USDA_shares
    weight[, , ] <- magpiesort(time_interpolate(total_prod[, , ], interpolated_year = c(1960, 2015), extrapolation_type = "constant", integrate_interpolated_years = TRUE))[, getYears(USDA_shares), ]
    
    # aggregate shares
    mapping <- toolGetMapping("regionmappingUSDATFPISO.csv", where = "mrcommons")
    weight[USDA_shares[,,"AG_Labour", drop = TRUE] == 0] <- 0 # excluding shares that are zero in aggregation
    #Unusually high capital shares
    weight[c("BLZ","CRI","DOM","HND","JAM","MEX","NIC","PAN","SLV","JPN"),,]<-0
   
    country<-intersect(getCells(USDA_shares),intersect(getCells(weight),unique(mapping$ISO)))
    years<-intersect(getYears(USDA_shares),getYears(weight))
    mapping1<-mapping[mapping$ISO %in% country,]
    USDA_shares <- toolAggregate(USDA_shares[country,years,], rel = mapping1, weight = weight[country,years,], from = "ISO", to = "Region", dim = 1)
    
    # re-scale shares to sum of labor + capital costs
    USDA_shares <- USDA_shares / dimSums(USDA_shares, dim = 3)
    
    # GDP (US$PPP) per capita as independent variable
    gdp <- calcOutput("GDPpc", gdp = "PPP", aggregate = FALSE)[, , "SSP2"]
    country<-intersect(getCells(gdp),unique(mapping$ISO))
    mapping2<-mapping[mapping$ISO %in% country,]
    years<-intersect(getYears(gdp),getYears(weight))
    
    gdp <- toolAggregate(gdp[country,years,],rel=mapping2,weight=weight[country,years,"Machinery"],from="ISO",to="Region")
    
    # reducing to shared years
    years <- intersect(getYears(USDA_shares), getYears(gdp))
    gdp <- gdp[, years, ]
    getNames(gdp) <- "GDP_pc"
    USDA_shares <- USDA_shares[,years,]
    
    # setting up data frame
    variables <- mbind(USDA_shares, gdp)
    variables  <- as.data.frame(variables)[, 2:5]
    variables  <- reshape(variables, idvar = c("Region", "Year"), timevar = "Data1", direction = "wide")
    variables[,"Value.GDP_pc_log"] <- log(variables[,"Value.GDP_pc"], base = 10)
    variables <-variables[variables$Region != "AFRICA, Developed",]
    
    # regression for labor or capital cost share
    if(factor=="lab"){
    res1<-lm(Value.AG_Labour~Value.GDP_pc_log,variables)
    }else if(factor=="cap"){
    res1<-lm(Value.Machinery~Value.GDP_pc_log,variables)  
    }
    
    # create magclass object
    res <- new.magpie(names = c("slope", "intercept"),sets = c("Region", "Year", "coefficients"))
    res[,,"slope"] <- as.numeric(res1[["coefficients"]][2])
    res[,,"intercept"] <- as.numeric(res1[["coefficients"]][1])

    
  } else {
    stop("Data source not available")
  }
  
  
  return(list(x=res,
              weight=NULL,
              unit="Share", 
              description="Regression parameters for factor share (capital or labout) calculation based on log10(GDPpc)"))
}
