#' @title correctFoodSystemsDashboard
#' @description Correct FoodSystemsDashboard values that are questionable
#' @param x magpie object provided by the read function
#' @param subtype Switch between different levels
#' @return List of magpie objects  
#' @author David Chen

correctFoodSystemsDashboard <- function(x, subtype){
  
  if (subtype == "Industrial Processing Share"){
    
    x["MMR",,"Proportion of rice that is industrially processed"] <- 65
    x["MYS",,"Proportion of rice that is industrially processed"] <- 50
    x["CHN",,"Proportion of maize flour that is industrially processed"] <- 100
    x["CAN",,"Proportion of maize flour that is industrially processed"] <- 100
    x["SWZ",,"Proportion of maize flour that is industrially processed"] <- 100
    
  }
return(x)  

}
