#' @title downloadAndrijevic2019
#' @description This function downloads the governance indicator data available
#'              at https://github.com/marina-andrijevic/governance2019
#'
#' @author Felicitas Beier
#'
#' @param subtype file that should be downloaded from governance2019 repository
#'
#' @seealso
#' [readAndrijevic2019()]
#'
#' @examples
#' \dontrun{
#' downloadSource("Andrijevic2019", subtype = "governance_obs_project")
#' }
#'
#' @importFrom madrat toolSubtypeSelect
#' @importFrom utils download.file

downloadAndrijevic2019 <- function(subtype = "governance_obs_project") {

  link  <- "https://raw.githubusercontent.com/marina-andrijevic/governance2019/master/data/"
  files <- c(governance_obs_project = "governance_obs_project.csv",
             master_proj_obs        = "master_proj_obs.csv")
  file  <- toolSubtypeSelect(subtype, files)

  download.file(url = paste0(link, file), destfile = file, mode = "wb")

  return(list(url           = "https://github.com/marina-andrijevic/governance2019",
              doi           = "10.1038/s41893-019-0405-0",
              title         = "Governance in socioeconomic pathways and its role for future adaptive capacity",
              author        = "Marina Andrijevic, marina.andrijevic@hu-berlin.de",
              version       = "not available",
              release_date  = "not available",
              description   = "not available",
              license       = "not available",
              reference     = "not available",
              unit          = "Index")
  )
}
