#' @title downloadMehta2022
#' @description download Global Area Equipped for Irrigation Dataset 1900-2015 from Mehta et al. (2022)
#'
#' @author  Felicitas Beier
#' @seealso [downloadSource()] [readMehta2022()]
#' @examples
#' \dontrun{
#' a <- downloadSource()
#' }
#'
#' @importFrom utils download.file
#' @importFrom withr with_options

downloadMehta2022 <- function() {

  aeiURL <- "https://zenodo.org/record/6886564"

  years <- c(seq(1900, 1970, by = 10),
             seq(1980, 2015, by = 5))
  years1 <- years[years < 2000]
  years2 <- years[years >= 2000]

  files <- c(paste0("G_AEI_", years1, ".ASC"),
             paste0("G_AEI_", years2, ".asc"))

  maxcount <- 10
  count    <- 0
  for (file in files) {
    repeat {

      withr::with_options(list(timeout = NULL),
                          code = try(download.file(paste0(aeiURL, "/files/", file),
                                                   destfile = file, mode = "wb")))

      count <- count + 1
      if (file.exists(file) || count >= maxcount) {
        break
      }
    }
  }

  return(list(url          = aeiURL,
              doi          = "https://doi.org/10.5281/zenodo.6886564",
              title        = "Global Area Equipped for Irrigation Dataset 1900-2015",
              revision     = "2022",
              release_date = "2015-07-01",
              author       = "Mehta, Piyush;  Siebert, Stefan;
                              Kummu, Matti; Deng, Qinyu; Ali, Tariq;
                              Marston, Landon; Xie, Wei;  Davis, Kyle",
              license      = "Creative Commons Attribution 4.0 International"))
}
