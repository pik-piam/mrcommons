#' @title calcResFor2ndBioengery
#' @description Calculates the supply potential of Crop Residues for 2nd generation bioenergy
#' for future and different ssp scenarios
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @param products categorie (set) that should be reported, switch between "kres",
#' "res_crop" (sum over all "kres"), "res_wood" and "all"
#' @param product_aggr boolean, if product set should be summed up
#' @param add_off add a column with empty supply for no residues available for 2nd gen BE
#' @author Kristine Karstens
#' @seealso
#' [calcResFor2ndBioengery()]
#' @examples
#' \dontrun{
#' calcOutput("ResFor2ndBioengery")
#' }
#'
calcResFor2ndBioengery <- function(products = "all",
                                   product_aggr = TRUE, add_off = FALSE) { # nolint: object_name_linter.

  if (length(products) > 1) {

    out <- NULL
    for (item in products) {
      tmp <- calcOutput("ResFor2ndBioengery", products = item, product_aggr = FALSE,
                        add_off = add_off, aggregate = FALSE)
      out <- mbind(out, tmp)
    }

  } else {

    if (!(products %in% c("kres", "all", "res_wood", "res_crop"))) stop("This product type is not available.")
    magYears  <- findset("time")
    pastYears <- findset("past")

    oldReMIND     <- readSource("ResFor2ndBE", subtype = "oldReMIND", convert = TRUE)
    missingYears <- setdiff(magYears, getYears(oldReMIND))
    oldReMIND     <- time_interpolate(oldReMIND, missingYears, integrate_interpolated_years = TRUE,
                                      extrapolation_type = "constant")[, magYears, ]
    # set start years
    oldReMIND[, pastYears, ] <- 0

    newAgriSupply <- readSource("ResFor2ndBE", subtype = "newAgriSupply", convert = TRUE)
    missingYears <- setdiff(magYears, getYears(newAgriSupply))
    newAgriSupply <- time_interpolate(newAgriSupply, missingYears, integrate_interpolated_years = TRUE,
                                      extrapolation_type = "constant")[, magYears, ]
    # set start years
    newAgriSupply[, pastYears, ] <- 0

    scenarios <- getNames(newAgriSupply, dim = 2)

    if (products == "kres") {

      out <- newAgriSupply

    } else if (products == "res_crop") {

      out <- add_dimension(dimSums(newAgriSupply, dim = 3.1), dim = 3.1, nm = "res_crop", add = "residues")

    } else if (products == "res_wood") {

      out <- add_dimension(oldReMIND[, , "res_wood"], dim = 3.2, nm = scenarios, add = "scenario")

    } else {

      out <- mbind(newAgriSupply, add_dimension(oldReMIND[, , "res_wood"], dim = 3.2, nm = scenarios, add = "scenario"))

    }

    getSets(out) <- c("iso", "t", "residues", "scenario")

    if (add_off) {
      out          <- add_columns(out, addnm = c("off"), dim = 3.2)
      out[, , "off"] <- 0
    }
  }

  if (product_aggr) out <- dimSums(out, dim = "residues")

  rename <- c(kres           = "Agricultural",
              res_crop       = "Agricultural",
              res_wood       = "Forestry",
              all            = "Agricultural and forestry")

  renamedProducts <- NULL
  for (item in products) renamedProducts <- rbind(renamedProducts, toolSubtypeSelect(item, rename))

  return(list(x = out,
              weight = NULL,
              unit = "except generalizable energy in PJ",
              description = paste0(paste(renamedProducts, collapse = " and "),
                                   " residues potentially available for 2nd generation bio energy")))
}
