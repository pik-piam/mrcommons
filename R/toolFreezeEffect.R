#' @title toolFreezeEffect
#' @description This function freeze values given a specific year and optionally additionally at the first non-zero value
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

    out                <- x
    reset_years        <- getYears(x, as.integer = TRUE) >= year
    out[, reset_years, ] <- setYears(x[, rep(year, sum(reset_years)), ], getYears(x[, reset_years, ]))

    if (constrain == "first_use") {
      # determine year of first use (as index in year dim (1 <=> first year))
      first_value <- first_use   <- toolConditionalReplace(magpply(x[, reset_years, ], found_first <- function(x) {
return(which(x != 0)[1])
}, c(1, 3)), "is.na()", 1)
      first_use   <- first_use + length(which(getYears(x, as.integer = TRUE) < year))

      # determine value of first use
      ncells <- length(getCells(x))
      ndata  <- length(getNames(x))
      nyears <- length(getYears(x))
      first_value[] <- x[as.array((ncells * nyears) * (rep(1:ndata, each = ncells) - 1) + ncells * (first_use - 1) + rep(1:ncells, times = ndata))]

      # set value of first usage for all later appearing later non-zero values
      out[as.array(out == 0 & x != 0)] <- first_value[, rep(1, nyears(x)), ][as.array(out == 0 & x != 0)]
    }

  return(out)
}
