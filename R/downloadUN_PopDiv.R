#' Download UN_PopDiv
#'
#' Download UN_PopDiv dataset (World Population Prospects)
#'
#' @author  Michael Crawford, Debbora Leip
#' @seealso  [madrat::downloadSource()] [mrdrivers::readUN_PopDiv()]
#' @examples
#' \dontrun{
#' a <- downloadSource()
#' }

#' @importFrom utils download.file unzip
downloadUN_PopDiv <- function() {  #nolint: object_name_linter.

  urlUN <- "https://population.un.org/wpp/Download/Files/5_Archive/WPP2015-Excel-files.zip"
  download.file(urlUN, destfile = "UN_PopDiv.zip")
  unzip("UN_PopDiv.zip", exdir = "UN_PopDiv_unzipped")

  # Grab required data from zip archive
  file.rename(from = "UN_PopDiv_unzipped/EXCEL_FILES/1_Population/WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS",
              to   = "WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS")

  unlink("UN_PopDiv.zip")                          # Remove zip file
  unlink("UN_PopDiv_unzipped", recursive = TRUE)   # Remove all unused datasets

  return(list(url          = urlUN,
              title        = "World Population Prospects",
              revision     = "2015",
              comment      = "Total population (both sexes combined) by major area, region and country,
                              annually for 1950-2100 (thousands)",
              release_date = "2015-07-01",
              author       = "United Nations, Department of Economic and Social Affairs, Population Division",
              license      = "Creative Commons Attribution-3.0 IGO (CC BY 3.0 IGO)")
  )

}
