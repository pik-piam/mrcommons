#' Calculate historical distribution of livestock production across
#' different systems based on output of MAgPIE_FEED model
#'
#' @param faoVersion which version of FAO food balances to use in mass balance
#' @return Historical distribution of livestock production across
#' different systems and corresponding weights as a list of two MAgPIE objects
#'
#' @author Isabelle Weindl
#' @seealso [madrat::calcOutput()], [readFeedModel()]
#' @examples
#' \dontrun{
#' calcOutput("ProdSysRatioPast")
#' }
calcProdSysRatioPast <- function(faoVersion = "join2010") {

  if (faoVersion == "join2010") {
    past <- findset("past_til2020")
  } else if (faoVersion == "pre2010") {
    past <- findset("past")
  } else if (faoVersion == "post2010") {
    past <- c("y2010", "y2015", "y2020")
  }
  # read in data
  prodsysratio <-  readSource(type = "FeedModel", subtype = "ProdSysRatio")

  #Extend historical data by filling in missing years with constant values
  missingYears <- setdiff(past, getYears(prodsysratio))
  if (length(missingYears) > 0) {
    prodsysratio <- toolHoldConstant(prodsysratio, years = missingYears)
  }

  # use livestock production as weight
  kli <- findset("kli")
  massbalance <- calcOutput("FAOmassbalance_pre", version = faoVersion, aggregate = FALSE)[, past, ]
  weight <- collapseNames(massbalance[, , kli][, , "dm"][, , "production"])

  mapping <- data.frame(kli = c("livst_pig", "livst_rum", "livst_chick", "livst_egg", "livst_milk"),
                        sys = c("sys_pig", "sys_beef", "sys_chicken", "sys_hen", "sys_dairy"),
                        stringsAsFactors = FALSE)

  weight <- luscale::rename_dimnames(weight, dim = 3, query = mapping, from = "kli", to = "sys")

  # remove datasets with NAs in weight/data
  prodsysratio <- toolNAreplace(x = prodsysratio, weight = weight, replaceby = 0)
  weight <- prodsysratio$weight
  out <- prodsysratio$x

  return(list(x = out,
              weight = weight,
              unit = "-",
              description = "Historical distribution of livestock production across different systems."))
}
