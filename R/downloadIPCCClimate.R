#' @title downloadIPCCClimate
#' @description Download IPCC climate classification
#'
#' @return Meta information on downloaded data
#' @author  Kristine Karstens
#' @examples
#'
#' \dontrun{
#' readSource("IPCCClimate", convert="onlycorrect")
#' }
#'
#' @importFrom utils download.file unzip

downloadIPCCClimate <- function() {

  # Define meta data
  meta <-  list(title = "Thematic Data Layers for Commission Decision of [10 June 2010] on
                         guidelines for the calculation of land carbon stocks for the purpose
                         of Annex V to Directive 2009/28/EC",
                url   = "https://esdac.jrc.ec.europa.eu/projects/RenewableEnergy/Data/Climate_Zone.zip")

  download.file(meta$url, destfile = "ipccclimate.zip")
  unzip("ipccclimate.zip")
  unlink("ipccclimate.zip")

  # Compose meta data by adding elements that are the same for all subtypes.
  return(list(url           = meta$url,
              doi           = NULL,
              title         = meta$title,
              author        = NULL,
              version       = NULL,
              release_date  = NULL,
              license       = NULL,
              reference     = NULL)
  )
}
