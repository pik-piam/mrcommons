#' Read European Environment Agency (EEA) data
#'
#' Read-in European Environment Agency (EEA) data on ETS emissions as magclass object
#'
#'
#' @param subtype data subtype. Either "ETS", "ESR", "total", "sectoral", "projections", or "projections-detailed"
#' @return magpie object of European Environment Agency (EEA) ETS emissions (GtCO2)
#' @author Renato Rodrigues, Falk Benke, Robin Hasse
#' @examples
#' \dontrun{
#' a <- readSource(type = "EEA_EuropeanEnvironmentAgency", subtype = "ETS")
#' }
#'
#' @importFrom dplyr left_join select filter mutate relocate %>% .data
#' @importFrom tidyr replace_na
#' @importFrom magclass as.magpie collapseDim
#' @importFrom quitte calc_addVariable
#' @importFrom readxl read_excel excel_sheets read_xlsx
#' @importFrom reshape2 melt
#' @importFrom madrat toolCountry2isocode
#' @importFrom utils read.csv

readEEA_EuropeanEnvironmentAgency <- function(subtype) { # nolint: object_name_linter.
  if (subtype == "ETS") {
    # v44: 2020 data, v38: 2019 data
    data <- read.csv("ETS_Database_v44.csv", sep = "\t")
    data <- data[, -c(2, 5)]
    data$year <- suppressWarnings(as.numeric(as.character(data$year)))
    data <- data[(!(is.na(data$year))), ]
    colnames(data) <- c("region", "ETS_info", "sector", "value", "period")
    data$region <- toolCountry2isocode(data$region, warn = FALSE)
    data <- data[(!(is.na(data$region))), ]
    data$ETS_info <- gsub(pattern = "\\.", replacement = "_", data$ETS_info)
    data$sector <- gsub(pattern = "\\.", replacement = "", data$sector)
    data$value <- as.numeric(gsub(" ", "", data$value)) / 1000000
    data <- data[, c(1, 5, 2, 3, 4)]
    x <- as.magpie(data, spatial = 1, temporal = 2, datacol = 5)

  } else if (subtype == "ESR") {
    # 2020 data
    data <- read_excel(path = "EEA_GHG_ESD_Dec 2020.xlsx", trim_ws = TRUE)
    data <- data[, c(1:3)]
    colnames(data) <- c("region", "period", "value")
    data$region <- toolCountry2isocode(data$region, warn = FALSE)
    data <- data[(!(is.na(data$region))), ]
    data$variable <- "Emi|GHG|ESR (Mt CO2-equiv/yr)"
    data <- data[, c(1, 2, 4, 3)]
    x <- as.magpie(data, spatial = 1, temporal = 2, datacol = 4)

  } else if (subtype == "total") {
    data <- read_excel(path = "GHG_Total_historical.xlsx", trim_ws = TRUE, .name_repair = "unique_quiet")
    data$...1 <- NULL
    eur <- c(
      "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL",
      "ITA", "LVA", "LIE", "LTU", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE",
      "CHE", "TUR", "GBR"
    )
    colnames(data) <- c("year", eur)
    data <- melt(data, id.vars = 1)
    colnames(data) <- c("year", "region", "value")
    data <- cbind(data, variable = c("Emi|GHGtot"))
    data$variable <- paste0(data$variable, sep = " ", "(Mt CO2-equiv/yr)")
    data <- data[, c(1, 2, 4, 3)]
    x <- as.magpie(data, spatial = 2, temporal = 1, datacol = 4)

  } else if (subtype == "sectoral") {
    sheets <- excel_sheets("GHG_ETS_ES_Projections_by_sector.xlsx")
    historical <- NULL
    timeframe <- seq(2005, 2017) # excluding WEM projections

    for (s in sheets) {
      tmp <- suppressMessages(read_excel(path = "GHG_ETS_ES_Projections_by_sector.xlsx",
                                         sheet = s, skip = 1, trim_ws = TRUE)) %>%
        melt(id.vars = 1) %>%
        mutate(value = replace_na(suppressWarnings(as.numeric(.data[["value"]])), 0))
      colnames(tmp) <- c("label", "period", "value")
      tmp <- cbind(tmp[!is.na(tmp$value) & tmp$period %in% timeframe, ], region = s)
      historical <- rbind(historical, tmp)
    }

    mappingVariable <- as.data.frame(
      cbind(
        variable = c(
          "Emi|GHG|ETS",
          "Emi|GHG|Energy|ETS",
          "Emi|GHG|Industry|ETS",
          "Emi|GHG|ESR",
          "Emi|GHG|Transport|ESR",
          "Emi|GHG|Buildings|ESR",
          "Emi|GHG|Industry|ESR",
          "Emi|GHG|Agriculture|ESR",
          "Emi|GHG|Waste|ESR"
        ),
        label = c(
          "Emissions Trading System (stationary installations)",
          "Energy Industries",
          "Other stationary installations",
          "Effort Sharing Decision and Regulation",
          "Transport",
          "Buildings",
          "Industry and other",
          "Agriculture",
          "Waste"
        )
      )
    )

    historical <- left_join(mappingVariable, historical, by = c("label"))
    historical$variable <- paste0(historical$variable, sep = " ", "(Mt CO2-equiv/yr)")
    historical$value <- as.double(historical$value)
    historical$label <- NULL
    historical <- historical[, c(1, 4, 2, 3)]
    x <- as.magpie(historical, spatial = 2, datacol = 4, temporal = 3)

  } else if (subtype == "projections") {

    projections <- read.csv(file = "GHG_projections/GHG_projections_2021_EEA.csv",
                            stringsAsFactors = FALSE, strip.white = TRUE) %>%
      filter(!!sym("CountryCode") != "",
             !!sym("CountryCode") != "EU",
             !!sym("Final.Gap.filled") != as.double(0),
             !is.na(!!sym("Final.Gap.filled"))) %>%
      select("CountryCode", "Year", "Category", "Scenario", "Gas", "Value" = "Final.Gap.filled") %>%
      mutate(Year = as.numeric(!!sym("Year")),
             Value = as.numeric(!!sym("Value"))) %>%
      relocate("Scenario", .after = "Year")

    x <- as.magpie(projections, spatial = 1, temporal = 2, datacol = 6)

  } else if (subtype == "projections-detailed") {

    path <- file.path("GHG_projections_detailed", "2023")
    files <- list.files(path = path)

    projections <- NULL
    for (file in files) {
      reg <- gsub("-.*", "", file)
      projections <- rbind(
        projections,
        suppressWarnings(read_xlsx(file.path(path, file))) %>%
          filter(!is.na(.data$Value), .data$Value != 0) %>%
          select(-"InventorySubmissionYear", -"Notation") %>%
          mutate("CountryCode" = reg)
      )
    }

    projections <- projections %>%
      mutate(Year = as.numeric(.data$Year),
             Value = as.numeric(.data$Value)) %>%
      # remove duplicates
      dplyr::group_by(.data$Category, .data$Scenario, .data$Year, .data$Gas, .data$CountryCode) %>%
      mutate("dup" = n()) %>%
      dplyr::ungroup() %>%
      filter(.data$dup == 1 | .data$RY == 1) %>%
      select(7, 4, 2, 1, 5, 6) %>%
      distinct()

    x <- as.magpie(projections, spatial = 1, temporal = 2, datacol = 6)

  } else if (subtype == "ghgEmissionIntensityElec") {
    # Greenhouse gas emission intensity of electricity generation in Europe
    # https://www.eea.europa.eu/ims/greenhouse-gas-emission-intensity-of-1
    x <- read.csv("co2-emission-intensity-15.csv") %>%
      select(period = 1, region = 2, value = 3) %>%
      filter(.data[["region"]] != "EU-27") %>%
      as.magpie(spatial = "region", temporal = "period", datacol = "value") %>%
      collapseDim()

  } else {
    stop("Not a valid subtype!")
  }

  return(x)
}
