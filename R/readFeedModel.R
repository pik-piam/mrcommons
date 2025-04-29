#' Read in data of FeedModel
#'
#' Read in csv files containing data on production system distribution and system-specific feed baskets from the
#' FeedModel "MAgPIE_FEED"
#'
#' @param subtype Available subtypes: "ProdSysRatio", "FeedBaskets" and "FeedBasketsDetailed"
#' @return magpie object of feed basket data
#' @author Isabelle Weindl
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{
#' a <- readSource(type = "FeedModel", subtype = "FeedBaskets")
#' }
#' @importFrom magclass read.magpie
readFeedModel <- function(subtype = "FeedBaskets") {

  folder <- "Version_2024_04_22/"

  files <- c(ProdSysRatio = "prod_sys_ratio.csv",
             FeedBaskets = "feed_bask_sys.csv",
             FeedBasketsDetailed = "feed_eff_DM_detailed.csv")

  file <- toolSubtypeSelect(subtype, files)
  x    <- read.magpie(paste0(folder, file))

  return(x)
}
