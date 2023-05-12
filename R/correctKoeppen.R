#' @title correctKoeppen
#' @description Correct Koeppen climate zones on cellular level
#' @param x magpie object provided by the read function
#' @param subtype Switch between different levels
#' @return List of magpie objects with results on cellular level
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#' readSource("Koeppen", subtype = "cellular", convert = "onlycorrect")
#' }
correctKoeppen <- function(x, subtype = "iso") {

  if (subtype == "cellular") {

    if (any(noZone <- dimSums(x, dim = 3) == 0)) {

      cat("Some cells do not have a value and will be filled by proxies.")

      # Filling island states with proxys based on latitude
      lat <- as.numeric(gsub("(.*)p(.*)", "\\1", getItems(x, dim = "y", full = TRUE)))
      afZone  <- abs(lat) <= 25
      cfaZone <- abs(lat) > 25 & abs(lat) <= 50
      etZone  <- abs(lat) > 50

      x[, , "Af"]  <- x[, , "Af"]  + afZone * noZone  # add 1 for all missing Af zone cells
      x[, , "Cfa"] <- x[, , "Cfa"] + cfaZone * noZone # add 1 for all missing Cfa zone cells
      x[, , "ET"]  <- x[, , "ET"]  + etZone * noZone  # add 1 for all missing ET zone cells

    }

    if (any(dimSums(x, dim = 3) == 0)) {
      vcat(1, "Some cells do not have a value and cannot be filled by proxies.")
    }
  }

  return(x)
}
