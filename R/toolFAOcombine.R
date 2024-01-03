#' Combine FAO datasets
#'
#' Allows to combine two similar FAO datasets with dublicates being removed.
#' For instance combine Production:Crops and Production: Crops Processed to one
#' magpie object
#'
#'
#' @param ... two magpie objects with FAO data
#' @param combine "Item" to combine datasets that for instance both contain
#' palm oil data
#' @return MAgPIE object with data from both inputs but dublicates removed
#' @author Ulrich Kreidenweis
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- toolFAOcombine(Crop, CropPro, combine = "Item")
#' }
#'
#' @importFrom magclass getNames getYears dimSums
#' @export
#'
toolFAOcombine <- function(..., combine = "Item") {
  x <- list(...)
  dotnames <- names(x)

  if (length(x) < 2) stop("At least two files have to be provided")
  if (length(x) > 2) stop("Function currently only working for two files")

  # first programmed for only two files (change to infinte number later)

  if (combine == "Item") {

    x1 <- x[[1]]
    x1[is.na(x1)] <- 0
    x2 <- x[[2]]
    x2[is.na(x2)] <- 0

    ## match temporal dimension
    years <- intersect(getYears(x1), getYears(x2))
    if (length(setdiff(getYears(x1), getYears(x2))) > 0) {
      vcat(1, "No data for year", gsub("y", "", setdiff(getYears(x1), getYears(x2))), "in", dotnames[2],
           "dataset. All data of this year removed. \n")
    }
    if (length(setdiff(getYears(x2), getYears(x1))) > 0) {
      vcat(1, "No data for year", gsub("y", "", setdiff(getYears(x2), getYears(x1))), "in", dotnames[1],
           "dataset. All data of this year removed. \n")
    }
    x1 <- x1[, years, ]
    x2 <- x2[, years, ]


    # items that occur in both datasets
    items1 <- getNames(x1)
    items2 <- getNames(x2)
    inboth <- intersect(items1, items2)


    if (length(inboth) > 0) {
      vcat(2, "For the following items there were values in both datasets:", inboth, "Only values from",
           dotnames[1], "dataset were considered")

      x1GLO <- dimSums(x1[, , inboth], dim = 1)
      x2GLO <- dimSums(x2[, , inboth], dim = 1)

      # short check if global sums of the values are the same.
      for (item in inboth) {
        avg <- mean((x1GLO[, , item] + 10^-8) / (x2GLO[, , item] + 10^-8))
        if (avg > 1.01 || avg < 0.99) {
          cat(0, "For", item, "the values in the two datasets seem to differ. Manual check recommended.")
        }
      }
    }

    # data2 without data already in data1 (CHECK!)
    x2rm <- x2[, , items2[!items2 %in% items1]]

  }

  if (combine == "Element") {

    stop("This option is not available yet", "\n")

  }

  bothtogether <- mbind(x1, x2rm)

  return(bothtogether)
}
