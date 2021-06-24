#' @title downloadAndrijevic2019
#' @description This function downloads the governance indicator data available
#'              at https://github.com/marina-andrijevic/governance2019
#'
#' @author Felicitas Beier
#'
#' @param subtype file that should be downloaded from governance2019 repository
#'
#' @seealso
#' \code{\link{readAndrijevic2019}}
#'
#' @examples
#' \dontrun{
#' downloadSource("Andrijevic2019", subtype = "governance_obs_project")
#' }
#'
#' @importFrom madrat toolSubtypeSelect
#' @importFrom utils download.file

downloadAndrijevic2019 <- function(subtype = "governance_obs_project") {

  link  <- "https://github.com/marina-andrijevic/governance2019/tree/master/data"
  files <- c(gdp_ssp_5year          = "gdp_ssp_5year.csv",
             governance_obs_project = "governance_obs_project.csv",
             master_proj_obs        = "master_proj_obs.csv",
             ndgain_readiness       = "ndgain_readiness.csv",
             observed_yr            = "observed_yr.csv",
             pop_size               = "pop_size.csv",
             projections_yr         = "projections_yr.csv",
             pwt70                  = "pwt70.csv",
             wgidataset             = "wgidataset.csv",
             wic_eduatt             = "wic_eduatt.csv",
             wic_mys_gap            = "wic_mys_gap.csv")
  file  <- toolSubtypeSelect(subtype, files)

  download.file(url = link, destfile = file, mode = "wb")
}
