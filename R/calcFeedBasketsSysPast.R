#' Calculate historical system-specific feed baskets based on output of MAgPIE_FEED model
#' as DM feed biomass (different types of feed) needed per DM livestock products of respective systems
#'
#'
#' @return Historical system-specific feed baskets and corresonding weights as a list of two MAgPIE
#' objects
#' @author Isabelle Weindl, Benjamin Bodirsky, Jan Philipp Dietrich
#' @seealso [madrat::calcOutput()], [readFeedModel()], [calcFeedBasketsPast()]
#' @examples
#' \dontrun{
#' calcOutput("FeedBasketsSysPast")
#' }
#' @importFrom magclass getNames
#' @importFrom luscale rename_dimnames

calcFeedBasketsSysPast <- function() {

  # read in system-specific feed basket data (for sys_dairy,sys_beef,sys_pig,sys_hen,sys_chicken)
  fbaskSys <-  readSource(type = "FeedModel", subtype = "FeedBaskets")


  # expand dim=3.2 to kall (add products like wood and woodfuel)
  kdiff               <- setdiff(findset("kall"), getNames(fbaskSys, dim = 2))
  fbaskSys            <- add_columns(fbaskSys, addnm = kdiff, dim = 3.2)
  fbaskSys[, , kdiff] <- 0

  # use livestock production as weight
  kli  <- findset("kli")
  past <- findset("past")
  massbalance <- calcOutput("FAOmassbalance_pre", aggregate = FALSE)[, past, ]
  weight <- collapseNames(massbalance[, , kli][, , "dm"][, , "production"])

  mapping <- data.frame(kli = c("livst_pig", "livst_rum", "livst_chick", "livst_egg", "livst_milk"),
                        sys = c("sys_pig", "sys_beef", "sys_chicken", "sys_hen", "sys_dairy"),
                        stringsAsFactors = FALSE)

  weight <- rename_dimnames(weight, dim = 3, query = mapping, from = "kli", to = "sys")

  # remove datasets with NAs in weight/data
  fbaskSys <- toolNAreplace(x = fbaskSys, weight = weight, replaceby = 0)
  weight   <- fbaskSys$weight
  out      <- fbaskSys$x

  return(list(x = out,
              weight = weight,
              unit = "1",
              description = "Detailed historical system-specific feed requirements
                             in DM per DM products generated for 5 livestock commodities."))
}
