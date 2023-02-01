#' @importFrom utils download.file tail

downloadLUH2v2 <- function(subtype = NULL) {
  links <- c("https://luh.umd.edu/LUH2/LUH2_v2h/states.nc",
             "https://luh.umd.edu/LUH2/LUH2_v2h/transitions.nc",
             "https://luh.umd.edu/LUH2/LUH2_v2h/management.nc",
             "https://luh.umd.edu/LUH2/LUH2_v2h/staticData_quarterdeg.nc")
  for (link in links) {
    fname <- tail(strsplit(link, split = "/")[[1]], 1)
    download.file(link, destfile = fname, mode = "wb")
  }
}
