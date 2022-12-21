#' @title calcFAOLand
#' @description Returns physical land areas from FAOSTAT
#'
#' @return land areas from FAOSTAT and weight
#' @author Ulrich Kreidenweis, Kristine Karstens

calcFAOLand <- function() {

  data <- readSource("FAO_online", "Land")
  data <- data[, , "1000_ha", pmatch = TRUE]   # subset all area information
  data <- collapseDim(data / 10^3, dim = 3.2)  # transform unit, drop unit statement

  return(list(x           = data,
              weight      = NULL,
              unit        = "mio. ha",
              description = "land-use categories from FAOSTAT"))
}
