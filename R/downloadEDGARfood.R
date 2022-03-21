#' @title downloadEDGARfood
#' @description download Edgar-food v6 food system emission data
#' @author David Hötten
#' @importFrom utils download.file bibentry
#'
downloadEDGARfood <- function() {
  url <- "https://edgar.jrc.ec.europa.eu/datasets/EDGAR-FOOD_v6.xlsx"
  download.file(url, "EDGAR-FOOD_v6.xlsx", method = "curl")
  meta <- list(
    title = "GHG food system emissions",
    url = "https://edgar.jrc.ec.europa.eu/datasets/EDGAR-FOOD_v6.xlsx",
    doi = "10.1038/s43016-021-00225-9",
    description = paste("GHG food system emissions (kt CO2eq, GWP-100 AR5),",
                        "including LULUC (Land use, Land Use Change)."),
    unit = "kt CO2eq",
    version = "v6",
    author  = person("Crippa", "Monica"),
    license = "most likely: CC BY 4.0", # https://figshare.com/articles/dataset/EDGAR-FOOD_emission_data/13476666
    reference = bibentry("Article",
                       title = "Food systems are responsible for a third of global anthropogenic GHG emissions",
                       author = c(person("Crippa, M"),
                                person("Solazzo, E"),
                                person("Guizzardi, D"),
                                person("Monforti-Ferrario, F"),
                                person("Tubiello, F N"),
                                person("Leip, A")),
                       year = "2021",
                       journal = "nature food",
                       url = "https://www.nature.com/articles/s43016-021-00225-9",
                       doi = "10.1038/s43016-021-00225-9")

  )

  return(meta)
}
