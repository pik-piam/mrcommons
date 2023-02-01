#' @title toolFreezeEffect
#' @description This function freeze values given a specific year and optionally additionally at the first
#'              non-zero value
#'
#' @param x data set to freeze
#' @param year year to hold constant (onwards)
#' @param constrain if FALSE, no constrain. Other options: 'first_use' (freeze from 'first use' ( <=> !=0 ))
#'
#' @return magpie object with global parameters
#' @author Kristine Karstens
#'
#' @export

toolFreezeEffect <- function(x, year, constrain = FALSE) {

  out                 <- x
  resetYears          <- getYears(x, as.integer = TRUE) >= year
  out[, resetYears, ] <- setYears(x[, rep(year, sum(resetYears)), ], getYears(x[, resetYears, ]))

  if (constrain == "first_use") {
    # determine year of first use (as index in year dim (1 <=> first year))
    firstValue <- firstUse <- toolConditionalReplace(
      magpply(x[, resetYears, ],
              function(x) {
                return(which(x != 0)[1])
              },
              c(1, 3)),
      "is.na()",
      1)
    firstUse   <- firstUse + length(which(getYears(x, as.integer = TRUE) < year))

    # determine value of first use
    ncells <- length(getCells(x))
    ndata  <- length(getNames(x))
    nyears <- length(getYears(x))
    firstValue[] <- x[as.array((ncells * nyears) * (rep(1:ndata, each = ncells) - 1)
                               + ncells * (firstUse - 1) + rep(1:ncells, times = ndata))]

    # set value of first usage for all later appearing later non-zero values
    out[as.array(out == 0 & x != 0)] <- firstValue[, rep(1, nyears(x)), ][as.array(out == 0 & x != 0)]
  }

  return(out)
}
