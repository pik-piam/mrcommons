#' @title downloadTFPUSDA
#' @description Downloads data of input shares based on a TFP assessment from USDA.
#'
#'
#'
#'
#'
#' @return raw TFP file from USDA
#' @author Edna J. Molina Bacca
#' @importFrom utils download.file
#' @seealso [downloadSource()]
#' @examples
#' \dontrun{
#' a <- download("TFP_USD")
#' }
#'
downloadTFPUSDA <- function() {

  # Agricultural total factor productivity growth indices for individual countries, 1961-2016
  # more information at: https://www.ers.usda.gov/data-products/international-agricultural-productivity/
  #:~:text=One%20of%20the%20most%20informative,of%20crop%20and%20livestock%20output.

  download.file("https://www.ers.usda.gov/webdocs/DataFiles/51270/AgTFPInternational2019.xlsx?v=263.4",
                "AgTFPindividualcountries.xlsx", mode = "wb")

  return(list(url           = "https://www.ers.usda.gov/webdocs/DataFiles/51270/AgTFPInternational2019.xlsx?v=263.4",
              doi           = "not available",
              title         = "Agricultural total factor productivity growth indices for individual countries, 1961-2016",
              author        = person("Keith Fuglie", "keith.fuglie@usda.gov"),
              version       = "not available",
              release_date  = "22/10/2021",
              description   = "One of the most informative measures of agricultural productivity is total factor productivity (TFP).
              TFP takes into account all of the land, labor, capital, and material resources employed in farm production and compares
              them with the total amount of crop and livestock output.",
              license       = "not available",
              reference     = "not available",
              unit          = "fraction"))
}
