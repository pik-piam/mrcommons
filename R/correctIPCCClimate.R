#' @title correctIPCCClimate
#' @description Correct IPCC climate classification
#'
#' @return Magpie object with results on cellular level for 12 IPCC climate zone types
#' @param x magpie object provided by the read function
#' @author  Kristine Karstens
#' @examples
#' \dontrun{
#' readSource("IPCCClimate", convert = "onlycorrect")
#' }
#'
correctIPCCClimate <-  function(x) {

  ### determine gaps on iso level
  iso <- where(setYears(x[, , "unknown"], "y2010") == 1)$true$regions
  iso <- unique(stringr::str_split(iso, "\\.", simplify = TRUE)[, 3])

  ## move unknown cells to most common country values
  nmax        <- rep(0, length(iso))
  names(nmax) <- iso
  for (i in iso) {
    # determine most common country value as proxy
    nmax[i] <- which.max(dimSums((x[i, ,  c(1:12)]), dim = 1))
    # setting most common value to unknown, if no data is available for th whole country
    nmax[i] <- ifelse(dimSums((x[i, ,  c(1:12)]), dim = c(1, 3)) == 0, 13, nmax[i])
    # only reassign cells that are actually unknown
    if (nmax[i] < 13) {
      unknownCells <- where(x[i, , "unknown"] == 1)$true$regions
      x[unknownCells, , nmax[i]] <- 1
      x[unknownCells, , "unknown"] <- 0
    } else if (i %in% c("PCN", "SHN", "IOT")) {
      unknownCells <- where(x[i, , "unknown"] == 1)$true$regions
      x[unknownCells, , "Tropical Wet"] <- 1
      x[unknownCells, , "unknown"] <- 0
    }
  }

  if (any(x[, , "unknown"] != 0)) {
    message("Still grid cells without climate zone")
  }

  x <- x[, , "unknown", invert = TRUE]

  return(x)
}
