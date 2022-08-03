#' @title calcHistBioplasticProd
#' @description calculates historic bioplastic production based on data and linear extrapolation
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("HistBioplasticProd")
#' }

calcHistBioplasticProd <- function() {

  globalProd <- readSource("HistBioplasticProd")

  # linear extrapolation to the past
  globalProd <- time_interpolate(globalProd,
                                 interpolated_year = c(2010, 2015),
                                 integrate_interpolated_years = TRUE,
                                 extrapolation_type = "linear")

  getNames(globalProd) <- NULL

  return(list(x = globalProd,
              weight = NULL,
              unit = "mio. t",
              description = "Global production of bioplastics"))
}
