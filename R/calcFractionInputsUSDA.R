#' @title calcFractionInputsUSDA
#' @description Calculates the factor factor shares for crop production from USDA'S Inputs shares.
#'
#'
#' @param products either "kcr" for crops, or "kli" for livestock
#' @return magpie object of the shares of the factor requirements in agriculture (capital, labor, materials, land).
#' @author Edna J. Molina Bacca, Debbora Leip
#' @importFrom dplyr  intersect
#' @importFrom magclass magpiesort
#' @importFrom magclass time_interpolate
#' @importFrom magclass collapseDim
#' @importFrom magclass collapseNames
#'
#'
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("FractionInputsUSDA")
#' }
#'
calcFractionInputsUSDA <- function(products = "kcr") {

  # value of animals is directly covered in MAgPIE
  TFP_shares_raw <- readSource("TFPUSDA")[, , "Livestock", invert = TRUE]

  # assuming the same share in the middle of the decade
  TFP_shares <- magpiesort(time_interpolate(TFP_shares_raw,
                                            interpolated_year = c((getYears(TFP_shares_raw, as.integer = TRUE) + 5)),
                                            extrapolation_type = "constant", integrate_interpolated_years = TRUE))

  # reads value of production
  VoP_All <- readSource("FAO_online", "ValueOfProd")

  # mio. USD ton. VoP for crops
  crop_prod_vop <- VoP_All[, ,  "2041|Crops.Gross_Production_Value_(constant_2014_2016_thousand_US$)_(1000_US$)"]
  # mio. USD ton. VoP for livestock
  lvst_prod_vop <- VoP_All[, , "2044|Livestock.Gross_Production_Value_(constant_2014_2016_thousand_US$)_(1000_US$)"]

  # costs division between crops and livestock
  shared_input <- c("Machinery", "AG_Labour") # factors that convene livestock and crops production
  crop_only <- c("AG_Land", "Materials_Crops") # inputs assumed to be dedicated specifically to crop production
  lvst_only <- c("Materials_Animals") # inputs assumed to be dedicated specifically to livestock production

  # years to cover
  years <- intersect(getYears(TFP_shares), getYears(VoP_All))

  # function to calculate shares
  .calc_fraction_shared <- function(cost_item, share_VoP, TFP_shares, total_input) {
    fraction <- share_VoP[, years, ] * TFP_shares[, years, cost_item] / total_input
    fraction[!is.finite(fraction)] <- 0
    return(fraction)
  }

  # calculating shares
  if (products == "kcr") {
    # Share of value of production between livestock and crop production
    share_VoP_total <- collapseNames(crop_prod_vop / (crop_prod_vop + lvst_prod_vop))
    share_VoP_total[!is.finite(share_VoP_total)] <- 0
    # just to make sure that shares are zero when there is no production
    share_VoP_crop <- collapseNames(crop_prod_vop / crop_prod_vop)
    share_VoP_crop[!is.finite(share_VoP_crop)] <- 0

    # to normalize overall summation of considered inputs
    total_input <- share_VoP_total[, years, ] * dimSums(TFP_shares[, years, shared_input], dim = 3) +
                      share_VoP_crop[, years, ] * dimSums(TFP_shares[, years, crop_only], dim = 3)

    # calculate shares
    shared_items <- mbind(lapply(shared_input, .calc_fraction_shared, share_VoP_total, TFP_shares, total_input))
    crop_items <- mbind(lapply(crop_only, .calc_fraction_shared, share_VoP_crop, TFP_shares, total_input))

    x <- mbind(shared_items, crop_items)
    getNames(x) <- c("Capital", "Labor", "Land", "Materials")

    # Production as weight
    Production <- dimSums(collapseDim(calcOutput("Production", products = "kcr", aggregate = FALSE)[, , "dm"]), dim = 3)

  } else if (products == "kli") {
    # Share of value of production between livestock and crop production
    share_VoP_total <- collapseNames(lvst_prod_vop / (crop_prod_vop + lvst_prod_vop))
    share_VoP_total[!is.finite(share_VoP_total)] <- 0
    # just to make sure that shares are zero when there is no production
    share_VoP_lvst <- collapseNames(lvst_prod_vop / lvst_prod_vop)
    share_VoP_lvst[!is.finite(share_VoP_lvst)] <- 0

    # to normalize overall summation of considered inputs
    total_input <- share_VoP_total[, years, ] * dimSums(TFP_shares[, years, shared_input], dim = 3) +
                     share_VoP_lvst[, years, ] * dimSums(TFP_shares[, years, lvst_only], dim = 3)

    # calculate shares
    shared_items <- mbind(lapply(shared_input, .calc_fraction_shared, share_VoP_total, TFP_shares, total_input))
    lvst_itmes <- mbind(lapply(lvst_only, .calc_fraction_shared, share_VoP_lvst, TFP_shares, total_input))

    x <- mbind(shared_items, lvst_itmes)
    getNames(x) <- c("Capital", "Labor", "Materials")

    # Production as weight
    Production <- dimSums(collapseDim(calcOutput("Production", products = "kli", aggregate = FALSE)[, , "dm"]), dim = 3)

  } else {
    stop("Invalid product type")
  }

  weight <- x
  weight[, , ] <- magpiesort(time_interpolate(Production[, , ],
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
