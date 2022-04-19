#' @title downloadTravelTimeNelson2019
#' @description download Nelson 2019 paper
#' @author David M Chen
#' @importFrom utils download.file bibentry
#'
downloadTravelTimeNelson2019 <- function() {
  url <- "https://figshare.com/ndownloader/articles/7638134/versions/3"
  download.file(url, "TravelTimeNelson2019.zip", mode = "wb")
  unzip("TravelTimeNelson2019.zip", exdir = "TravelTimeNelson_unzipped")


   meta <- list(
    title = "Travel time to cities and ports in the year 2015",
    url = "https://figshare.com/ndownloader/articles/7638134/versions/3",
    doi = "10.6084/m9.figshare.7638134.v3",
    description = paste("GHG food system emissions (kt CO2eq, GWP-100 AR5),",
                        "including LULUC (Land use, Land Use Change)."),
    unit = "minutes",
    version = "3",
    author  = person("Nelson", "Andy"),
    license = "most likely: CC BY 4.0",
    reference = bibentry("Article",
                         title = "A suite of global accessibility indicators",
                         author = c(person("Nelson, A"),
                                    person("Weiss, D J"),
                                    person("v. Etten, J"),
                                    person("Cattaneo, A"),
                                    person("McMenomy, T S"),
                                    person("Koo, J")),
                         year = "2019",
                         journal = "Scientific Data",
                         url = "https://www.nature.com/articles/s41597-019-0265-5",
                         doi = "10.1038/s41597-019-0265-5")

  )
  return(meta)
}
