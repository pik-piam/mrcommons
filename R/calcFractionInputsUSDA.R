#' @title calcFractionInputsUSDA
#' @description Calculates the factor factor shares for crop production from USDA'S Inputs shares.
#'
#' @param products either "kcr" for crops, or "kli" for livestock
#' @return magpie object of the shares of the factor requirements in agriculture (capital, labor, materials, land).
#' @author Edna J. Molina Bacca, Debbora Leip
#' @importFrom dplyr  intersect
#' @importFrom magclass magpiesort
#' @importFrom magclass time_interpolate
#' @importFrom magclass collapseDim
#' @importFrom magclass collapseNames
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("FractionInputsUSDA")
#' }
#'
calcFractionInputsUSDA <- function(products = "kcr") {
  # value of animals is directly covered in MAgPIE
  tfpSharesRaw <- readSource("TFPUSDA")[, , "Livestock", invert = TRUE]

  # assuming the same share in the middle of the decade
  tfpShares <- magpiesort(time_interpolate(tfpSharesRaw,
                                            interpolated_year = c((getYears(tfpSharesRaw, as.integer = TRUE) + 5)),
                                            extrapolation_type = "constant", integrate_interpolated_years = TRUE))

  # reads value of production
  voPAll <- readSource("FAO_online", "ValueOfProd")

  # mio. USD ton. VoP for crops
  cropProdVop <- voPAll[, ,  "2041|Crops.Gross_Production_Value_(constant_2014_2016_thousand_US$)_(1000_US$)"]
  # mio. USD ton. VoP for livestock
  lvstProdVop <- voPAll[, , "2044|Livestock.Gross_Production_Value_(constant_2014_2016_thousand_US$)_(1000_US$)"]

  # costs division between crops and livestock
  sharedInput <- c("Machinery", "AG_Labour") # factors that convene livestock and crops production
  cropOnly <- c("AG_Land", "Materials_Crops") # inputs assumed to be dedicated specifically to crop production
  lvstOnly <- c("Materials_Animals") # inputs assumed to be dedicated specifically to livestock production

  # years to cover
  years <- intersect(getYears(tfpShares), getYears(voPAll))

  # function to calculate shares
  .calcFractionShared <- function(costItem, shareVoP, tfpShares, totalInput) {
    fraction <- shareVoP[, years, ] * tfpShares[, years, costItem] / totalInput
    fraction[!is.finite(fraction)] <- 0
    return(fraction)
  }

  # calculating shares
  if (products == "kcr") {
    # Share of value of production between livestock and crop production
    shareVoPtotal <- collapseNames(cropProdVop / (cropProdVop + lvstProdVop))
    shareVoPtotal[!is.finite(shareVoPtotal)] <- 0
    # just to make sure that shares are zero when there is no production
    shareVoPcrop <- collapseNames(cropProdVop / cropProdVop)
    shareVoPcrop[!is.finite(shareVoPcrop)] <- 0

    # to normalize overall summation of considered inputs
    totalInput <- shareVoPtotal[, years, ] * dimSums(tfpShares[, years, sharedInput], dim = 3) +
                      shareVoPcrop[, years, ] * dimSums(tfpShares[, years, cropOnly], dim = 3)

    # calculate shares
    sharedItems <- mbind(lapply(sharedInput, .calcFractionShared, shareVoPtotal, tfpShares, totalInput))
    cropItems <- mbind(lapply(cropOnly, .calcFractionShared, shareVoPcrop, tfpShares, totalInput))

    x <- mbind(sharedItems, cropItems)
    getNames(x) <- c("Capital", "Labor", "Land", "Materials")

    # production as weight
    production <- dimSums(collapseDim(calcOutput("Production", products = "kcr", aggregate = FALSE)[, , "dm"]), dim = 3)

  } else if (products == "kli") {
    # Share of value of production between livestock and crop production
    shareVoPtotal <- collapseNames(lvstProdVop / (cropProdVop + lvstProdVop))
    shareVoPtotal[!is.finite(shareVoPtotal)] <- 0
    # just to make sure that shares are zero when there is no production
    shareVoPlvst <- collapseNames(lvstProdVop / lvstProdVop)
    shareVoPlvst[!is.finite(shareVoPlvst)] <- 0

    # to normalize overall summation of considered inputs
    totalInput <- shareVoPtotal[, years, ] * dimSums(tfpShares[, years, sharedInput], dim = 3) +
                     shareVoPlvst[, years, ] * dimSums(tfpShares[, years, lvstOnly], dim = 3)

    # calculate shares
    sharedItems <- mbind(lapply(sharedInput, .calcFractionShared, shareVoPtotal, tfpShares, totalInput))
    lvstItmes <- mbind(lapply(lvstOnly, .calcFractionShared, shareVoPlvst, tfpShares, totalInput))

    x <- mbind(sharedItems, lvstItmes)
    getNames(x) <- c("Capital", "Labor", "Materials")

    # Production as weight
    production <- dimSums(collapseDim(calcOutput("Production", products = "kli", aggregate = FALSE)[, , "dm"]), dim = 3)

  } else {
    stop("Invalid product type")
  }

  weight <- x
  weight[, , ] <- magpiesort(time_interpolate(production[, , ],
                              interpolated_year = 2015, extrapolation_type = "constant",
                              integrate_interpolated_years = TRUE))[, getYears(x), ]
  weight[!is.finite(x)] <- 0
  weight[x == 0] <- 0

  return(list(x = x,
              weight = weight,
              mixed_aggregation = NULL,
              unit = "fraction",
              description = "Factor shares for crops from USDA TFP data"))

}
