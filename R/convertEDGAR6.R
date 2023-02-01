convertEDGAR6 <- function(x, subtype) {
  # fill all missing countries with 0
  getItems(x, dim = 1) <- sub(pattern = "ANT", replacement = "SXM", x = getItems(x, dim = 1))
  getItems(x, dim = 1) <- sub(pattern = "SCG", replacement = "SRB", x = getItems(x, dim = 1))

  x <- toolCountryFill(x, fill = 0)

  return(x)
}
