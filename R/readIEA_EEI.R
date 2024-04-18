#' Read-in data from IEA End Uses and Efficiency Indicators Database
#'
#' @author Falk Benke
#' @importFrom data.table fread
#' @importFrom dplyr %>% filter mutate distinct
#' @importFrom magclass as.magpie
readIEA_EEI <- function() { #nolint object_name_linter

  data <- NULL

  for (domain in c("INDUSTRY", "TRANSPORT", "RESIDENTIAL", "SERVICES")) {
    data <- rbind(data, fread(
      file = file.path("2023", paste0("IEA - EEI ", domain, ".TXT")),
      col.names = c("ITEM", "ENDUSE", "TIME", "COUNTRY", "VALUE"),
      colClasses = c("character", "character", "numeric", "character", "character"),
      sep = " ", stringsAsFactors = FALSE, na.strings = c("x", "..", "c"), skip = 0, showProgress = FALSE
    ))
  }

  data <- filter(data, !is.na(.data$VALUE)) %>%
    mutate("VALUE" = as.numeric(.data$VALUE)) %>%
    distinct()

  x <- as.magpie(data, spatial = "COUNTRY", temporal = "TIME")

  return(x)
}
