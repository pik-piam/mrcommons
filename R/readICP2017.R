#' @title readICP2017
#' @description  Reads data of World Bank ICP round, downloaded from here: https://databank.worldbank.org/source/icp-2017
#' Reads
#'
#' @param subtype data subtype to be read in. Available subtypes are:
#' \itemize{
#' \item `priceLevel` Price level index (World = 100)
#' \item `expRatio` Expenditure component share of GDP (GDP = 100%)
#' \item `exp_LCU` Expenditure (local currency units, billions)
#' \item `exp_MER` Expenditure, market exchange rate-based (US$, billions)
#' \item `exp_PPP` Expenditure, PPP-based (US$, billions)
#' \item `per_cap_expPPP` Expenditure per capita, PPP-based (US$)
#' \item `per_cap_expMER` Expenditure per capita, market exchange rate-based (US$)
#' }
#' @return magpie object of relative price levels (world = 100) or per capita expenditure (USD17 MER)
#' @author David M Chen
#' @importFrom dplyr %>% select mutate filter relocate
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' a <- readSource("ICP2017", "per_cap_exp")
#' }
#'
readICP2017 <- function(subtype = "per_cap_expMER") {

  file =  "418a4224-8694-47b3-a918-5200014d1728_Data.csv"

  subtypes <- c( priceLevel = "Price level index (World = 100)",
                 expRatio = "Expenditure component share of GDP (GDP = 100%)",
                 exp_LCU = "Expenditure (local currency units, billions)",
                 exp_MER = "Expenditure, market exchange rate-based (US$, billions)",
                 exp_PPP = "Expenditure, PPP-based (US$, billions)",
                 per_cap_expPPP = "Expenditure per capita, PPP-based (US$)",
                 per_cap_expMER = "Expenditure per capita, market exchange rate-based (US$)")

  # Subsetting based on type of requested output
  out <- toolSubtypeSelect(subtype, subtypes)

  # Reads data
  data <- read.csv(file = file)
  colnames(data) <- gsub(colnames(data),pattern = ".*YR", replacement = "")

  #take out last lines which have a download tag
  data <- data[1:(nrow(data)-5),]

  x   <-  data %>%
          select("Country.Code", "Series.Name", "Classification.Name", "2011.", "2012.", "2013.", "2014.", "2015.", "2016.", "2017."  ) %>%
          pivot_longer(names_to = "Year", cols = c(4:10)) %>%
          mutate("Product" = gsub(x = .data$Series.Name, pattern = ".*:", replacement = ""),
                 "Year" = as.integer(.data$Year),
                  "value" = as.numeric(gsub(x = .data$value, pattern = "\\.\\.", replacement = "0")),
                 "Region" = .data$Country.Code,
                 "Indicator" = .data$Classification.Name,
                 .keep = "unused") %>%
         filter(.data$Year %in% c(2011, 2017)) %>%
         relocate(.data$value, .after = .data$Indicator) %>%
        as.magpie(spatial= 3, temporal = 1, tidy = TRUE)

  #remove aggregate countries with toolCountryFill
  x <- toolCountryFill(x, fill = 0, no_remove_warning = c("BON", "EAB", "ECB",
                                                          "KSV", "LCB", "MEB",
                                                          "NAB", "SAB", "SSB",
                                                          "WLD"))

    return(x[,,out])
  }

