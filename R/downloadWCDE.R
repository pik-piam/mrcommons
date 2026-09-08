#' @title downloadWCDE
#' @description Downloads population data by age, sex and education from the
#' Wittgenstein Centre Human Capital Data Explorer (WCDE,
#' https://dataexplorer.wittgensteincentre.org). The batch files provided for
#' the wcde R package are used (one file per scenario).
#'
#' Subtype "epop_v3" contains the updated SSP projections (2020-2100) of the
#' Wittgenstein Centre Data Explorer Version 3 (K.C. et al. 2024). The download
#' is pinned to version 3.3 to keep results reproducible when newer minor
#' versions are released.
#' Subtype "epop_v2" contains the previous release (Version 2, Lutz et al.
#' 2018), which includes the historical reconstruction going back to 1950.
#' As historical estimates do not differ between scenarios, only SSP2 is
#' downloaded for "epop_v2".
#'
#' Additional scenarios (e.g. the SSP2 zero migration variant, scenario code
#' 22) can be added to the scenario vectors below.
#'
#' @param subtype "epop_v3" or "epop_v2"
#' @author Benjamin Leon Bodirsky
#' @seealso [madrat::downloadSource()] [readWCDE()]
#' @importFrom utils download.file
downloadWCDE <- function(subtype = "epop_v3") {

  referenceKC <- paste0("K.C., S., Dhakad, M., Potancokova, M., Adhikari, S., Yildiz, D., ",
                        "Mamolo, M., Sobotka, T., Zeman, K., Abel, G., Lutz, W. and Goujon, A. (2024). ",
                        "Updating the Shared Socioeconomic Pathways (SSPs) Global Population and ",
                        "Human Capital Projections. IIASA Working Paper WP-24-003.")
  referenceLutz <- paste0("Lutz, W., Goujon, A., K.C., S., Stonawski, M. and Stilianakis, N. (Eds.) (2018). ",
                          "Demographic and Human Capital Scenarios for the 21st Century: 2018 assessment ",
                          "for 201 countries. Publications Office of the European Union.")

  settings <- list(
    epop_v3 = list(
                   folder       = "wcde-v33-batch",
                   indicator    = "epop",
                   scenarios    = c(SSP1 = 1, SSP2 = 2, SSP3 = 3, SSP4 = 4, SSP5 = 5),
                   title        = "Wittgenstein Centre Data Explorer V3: Population Size by Education",
                   version      = "wcde-v3.3",
                   release_date = "2024-02-01",
                   doi          = "10.5281/zenodo.10618931",
                   reference    = referenceKC),

    epop_v2 = list(
                   folder       = "wcde-v2-batch",
                   indicator    = "epop",
                   scenarios    = c(SSP2 = 2),
                   title        = "Wittgenstein Centre Data Explorer V2: Population Size by Education",
                   version      = "wcde-v2",
                   release_date = "2018-01-01",
                   doi          = NULL,
                   reference    = referenceLutz)
  )
  meta <- toolSubtypeSelect(subtype, settings)

  servers <- c("https://wicshiny2023.iiasa.ac.at/wcde-data/",
               "https://github.com/guyabel/wcde-data/raw/master/")

  for (scenario in names(meta$scenarios)) {
    path <- paste0(meta$folder, "/", meta$scenarios[[scenario]], "/", meta$indicator, ".rds")
    destfile <- paste0(scenario, ".rds")
    success <- FALSE
    for (server in servers) {
      err <- try(suppressWarnings(download.file(paste0(server, path), destfile = destfile,
                                                mode = "wb", quiet = TRUE)), silent = TRUE)
      if (!inherits(err, "try-error") && file.exists(destfile) && file.size(destfile) > 0) {
        success <- TRUE
        break
      }
    }
    if (!success) stop("Download of \"", path, "\" failed on all servers!")
  }

  authorWitt <- "Wittgenstein Centre for Demography and Global Human Capital (IIASA, OeAW, University of Vienna)"
  return(list(url          = paste0(servers[1], meta$folder, "/"),
              doi          = meta$doi,
              title        = meta$title,
              author       = authorWitt,
              version      = meta$version,
              release_date = meta$release_date,
              description  = paste0("De facto population in a country by five-year age groups, sex and ",
                                    "highest level of educational attainment. Figures are in thousands."),
              unit         = "thousand people",
              license      = "Creative Commons Attribution 4.0 International (CC BY 4.0)",
              reference    = meta$reference)
  )
}
