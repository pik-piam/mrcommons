#' @title readFoodSystemsDashboard
#' @description read in Food Systems Dashboard indicators
#' @return governance index data at iso-country level
#'
#' @param subtype Currently either "Processed" for Processed food expenditures per capita or "Industrial Processing Share"
#'
#' @author David Chen
#'
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("FoodSystemsDashboard")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter select group_by arrange %>% mutate last
#' @importFrom stringr str_detect
#' @importFrom magclass as.magpie


readFoodSystemsDashboard <- function(subtype) {

  x <- read.csv("FSD-Records_Export_Complete_Data_20211022_1554.csv")

  extra_mapping <- c("Gambia The" = "GMB",
                     "St Vincent and the Grenadines" = "VCT",
                     "Palestinian Territory, Occupied" = "PSE",
                     "Holy See (Vatican City)" = "VAT")


  if (subtype == "Industrial Processing Share") {

  x2 <- filter(x, .data$Subsector == "Processing and packaging") %>%
              filter(str_detect(.data$Indicator, "industrially")) %>%
              filter(!.data$AreaName %in% c("Polynesia", "Micronesia")) %>%
              select(.data$AreaName, .data$TimePeriod, .data$Indicator,
                     .data$DataValue)



  x2$AreaName <- toolCountry2isocode(x2$AreaName, mapping = extra_mapping)
  x2 <- filter(x2, !is.na(.data$AreaName))
  x2$DataValue <- as.numeric(x2$DataValue)
  x2$TimePeriod <- as.numeric(x2$TimePeriod)

  x2 <- group_by(x2, .data$AreaName, .data$Indicator) %>%
        dplyr::arrange(.data$TimePeriod) %>%
    mutate(
      "latest" = last(na.omit(.data$DataValue)),
      "max" = max(na.omit(.data$DataValue)))

  ## USING LATEST VALUE (and not max) to have a single-year estimate

 x2  <- group_by(x2, .data$AreaName, .data$Indicator) %>%
         summarise(mean(.data$latest))

  out <- as.magpie(x2, spatial = "AreaName", temporal = NULL, tidy = TRUE)

  # Years centred around 2017
  getItems(out, dim = 2) <- "y2017"
  }

 if (subtype == "Processed") {

    proc <- read.csv("Retail_value_of_packaged_food_sales_per_capita.csv")
    ultr <- read.csv("ultra_processed_food_sales_per_capita.csv")

   .readProc <- function(x) {

    tmp <- x %>%
      filter(!.data$AreaName %in% c("Polynesia", "Micronesia")) %>%
      select(.data$AreaName, .data$TimePeriod, .data$Indicator,
             .data$DataValue)

    extra_mapping <- c(extra_mapping,
                       "St. Martin (French part)" = "MAF",
                       "Macau SAR" = "MAC")

    tmp$AreaName <- toolCountry2isocode(tmp$AreaName, mapping = extra_mapping)

    tmp <- filter(tmp, !is.na(.data$AreaName))
    tmp$DataValue <- as.numeric(tmp$DataValue)
    tmp$TimePeriod <- as.numeric(tmp$TimePeriod)

    tmp <- as.magpie(tmp, spatial = 1, temporal = 2, tidy = TRUE)
    return(tmp)
    }

    proc_out <- .readProc(proc)
    ultr_out <- .readProc(ultr)

    out <- mbind(proc_out, ultr_out)

  }
  return(out)

}
