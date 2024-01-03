convertEDGAR_LU <- function(x) { # nolint: object_name_linter.
  # fill all missing countries with 0
  x <- toolCountryFill(x, fill = 0)
  return(x)
}
