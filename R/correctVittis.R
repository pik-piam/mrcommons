#' @title correctVittis
#' @description Correct inconsistencies in crop naming in the Vittis dataset
#' @param x magpie object provided by the read function
#' @return corrected mapie object on national-scale costs of production for 10 
#' crops, disaggregated in 9 distinct cost elements
#' @author Debbora Leip
#' @seealso
#' \code{\link{readVittis}}

correctVittis <- function(x) {

  # fix inconsistent naming
  x[, , "whea"] <- dimSums(x[, , c("wheat", "whea")], dim = "CropType", na.rm = TRUE)
  x[, , "barl"] <- dimSums(x[, , c("bar", "barl")], dim = "CropType", na.rm = TRUE)
  x[, , "pota"] <- dimSums(x[, , c("potato", "pota")], dim = "CropType", na.rm = TRUE)
  x[, , "maiz"] <- dimSums(x[, , c("corn", "maiz")], dim = "CropType", na.rm = TRUE)

  # machinery costs for sorghum, rice, groundnut are not available, and therefore
  # Vittis provides machinery costs for crops the are assumed to have similar costs
  x[, , list("sorg", "Machinery")] <- x[, , list("oats", "Machinery")]
  x[, , list("rice", "Machinery")] <- x[, , list("barl", "Machinery")]
  x[, , list("grou", "Machinery")] <- x[, , list("rape", "Machinery")]

  # remove obsolete crop categories
  x <- x[, , c("wheat", "bar", "potato", "corn", "oats", "rape"), invert = TRUE]
  
  return(x)
}