#' Read IEA
#'
#' Read-in an IEA csv file as magpie object
#'
#' @param subtype data subtype. Either "EnergyBalances", "EnergyBalances-latest", or "Emissions".
#' - "EnergyBalances": IEA energy balances until 2022 (2023 incomplete), data updated in Sep 2024,
#' the current default for REMIND input data
#' - "EnergyBalances-latest": IEA energy balances until 2023 (2024 incomplete), data updated in Aug 2025,
#' - "EnergyBalances-2026": incomplete IEA energy balances update from Apr 2026,
#' @return magpie object of the IEA
#' @author Anastasis Giannousakis, Lavinia Baumstark, Renato Rodrigues, Falk Benke
#'
readIEA <- function(subtype) {
  if (subtype == "EnergyBalances") {
    energyBalancesFile <- "IEA-Energy-Balances-2024/worldbig.csv"
    incomplete <- 2023

    data <- data.table::fread(
      file = energyBalancesFile,
      col.names = c("COUNTRY", "PRODUCT", "FLOW", "TIME", "ktoe"),
      colClasses = c("character", "character", "character", "numeric", "character"),
      sep = ";", stringsAsFactors = FALSE, na.strings = c("x", "..", "c"), skip = 2, showProgress = FALSE
    )

    data$COUNTRY <- toolCountry2isocode(data$COUNTRY, warn = FALSE)

    data <- data %>%
      filter(
        !is.na(.data$ktoe),
        !is.na(.data$COUNTRY),
        .data$TIME != incomplete
      ) %>% # exclude latest year with incomplete data
      mutate("ktoe" = as.numeric(.data$ktoe))

    mdata <- as.magpie(data,
      datacol = dim(data)[2], spatial = which(colnames(data) == "COUNTRY"),
      temporal = which(colnames(data) == "TIME")
    )
  } else if (subtype == "EnergyBalances-latest") {
    data <- NULL
    incomplete <- 2024

    for (i in seq(1, 3)) {
      energyBalancesFile <- file.path("IEA-Energy-Balances-2025", paste0("WBIG", i, ".zip"))

      tmp <- data.table::fread(
        file = energyBalancesFile,
        col.names = c("COUNTRY", "PRODUCT", "FLOW", "TIME", "UNIT", "VALUE"),
        stringsAsFactors = FALSE,
        na.strings = c("x", "..", "c", ""),
        skip = 0,
        showProgress = FALSE,
        select = c(1, 2, 4, 3, 5, 6)
      ) %>%
        filter(
          .data$UNIT %in% c("KTOE", "GWH"),
          .data$TIME != incomplete # exclude latest year with incomplete data
        ) %>%
        mutate("VALUE" = as.numeric(.data$VALUE))


      tmp$COUNTRY <- toolCountry2isocode(tmp$COUNTRY, warn = TRUE, mapping = c(
        "BURKINAFASO" = "BFA",
        "CONGO_DRC" = "COD",
        "CONGO_REPUB" = "COG",
        "DOMINICANREP" = "DOM",
        "NEWZEALAND" = "NZL",
        "SAUDIARABIA" = "SAU",
        "SOUTHAFRICA" = "ZAF",
        "WORLD" = "GLO",
        "F_USSR" = "SUN",
        "F_YUGOSLAVIA" = "YUG",
        "OTH_NON_OECDAFR" = "IAF",
        "OTH_NON_OECDAM" = "ILA",
        "OTH_NON_OECDAO" = "IAS"
      ))

      tmp <- tmp %>%
        filter(!is.na(.data$COUNTRY), !is.na(.data$VALUE))

      data <- rbind(data, tmp)
      rm(tmp)
    }

    mdata <- as.magpie(data, spatial = 1, temporal = 4, datacol = 6)

  } else if (subtype == "EnergyBalances-2026") {
    data <- NULL
    incomplete <- 2025

    files <- list.files("IEA-Energy-Balances-2026-Apr", pattern = "*.csv", full.names = TRUE)

    for (f in files) {
      tmp <- data.table::fread(
        file = f,
        select = c(3, 7, 5, 11, 17, 15),
        col.names = c("COUNTRY", "PRODUCT", "FLOW", "TIME", "UNIT", "VALUE"),
        na.strings = c("x", "..", "c", ""),
        skip = 0,
        showProgress = FALSE
      ) %>%
        filter(
          .data$UNIT %in% c("KTOE", "GWH"),
          .data$TIME != incomplete # exclude latest year with incomplete data
        ) %>%
        mutate("VALUE" = as.numeric(.data$VALUE))

      tmp$COUNTRY <- madrat::toolCountry2isocode(tmp$COUNTRY,
        warn = TRUE,
        mapping = c(
          "NEWZEALAND" = "NZL",
          "SOUTHAFRICA" = "ZAF"
        )
      )
      tmp <- tmp %>%
        filter(!is.na(.data$COUNTRY), !is.na(.data$VALUE))
      data <- rbind(data, tmp)
      rm(tmp)
    }

    mdata <- as.magpie(data, spatial = 1, temporal = 4, datacol = 6)

  } else if (subtype == "Emissions") {
    data <- read.csv("emissions2013.csv")
    data$COUNTRY <- toolCountry2isocode(data$COUNTRY, warn = FALSE) # nolint
    data <- data[!is.na(data$COUNTRY), ]
    data$TIME <- paste("y", data$TIME, sep = "") # nolint
    if (names(data)[[5]] == "MtCO2") data <- data[!is.na(data$MtCO2), ]
    if (names(data)[[5]] == "MtCO2") data$MtCO2 <- suppressWarnings(as.numeric(data$MtCO2)) # nolint
    mdata <- as.magpie(data, datacol = dim(data)[2])
  } else {
    stop("Invalid subtype!")
  }
  return(magclass::magpiesort(mdata))
}
