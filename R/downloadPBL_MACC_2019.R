#' @importFrom utils download.file tail

downloadPBL_MACC_2019 <- function() { # nolint : object_name_linter.

  links <- c(mmc1.xlsx = "https://ars.els-cdn.com/content/image/1-s2.0-S2352340919306882-mmc1.xlsx",
             mmc2.xlsx = "https://ars.els-cdn.com/content/image/1-s2.0-S2352340919306882-mmc2.xlsx")

  ### execute downloading
  lapply(seq_along(links), FUN = function(x) {
 download.file(links[x], destfile = names(links)[x], mode = "wb")
})

}
