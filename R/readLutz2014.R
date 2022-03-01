#' @title readLutz2014
#' @description It reads and clears the dataset of the global population projections
#' by age, sex and education, available on the Wittgenstein Centre Data Explorer
#' and published by Lutz, Butz and K. C. , 2014. "Population and human capital
#' in the twenty-first century" Oxford University Press.
#' From .csv file to a magclass object
#'
#' @return magpie object with the dataset downloaded. It contains missing values
#' and it is possible to replace them with the function convertLutz2014.
#'
#' @seealso
#' [convertLutz2014()]
#'
#' @importFrom madrat toolCountry2isocode
#' @importFrom reshape2 acast
#' @importFrom utils read.table
readLutz2014 <- function() {
  merge <- list()
  for (scenario in paste0("SSP", 1:5)) {
    for (gender in c("Both", "M-F")) {
      filename <- paste0("wicdf ", scenario, " ", gender, ".csv")
      d <- read.table(file = filename, skip = 8, quote = '"', header = TRUE, sep = ",")

      if (length(d) == 5) { # "Both"
        target <- which(names(d) == "Age")
        d <- cbind(d[, seq_len(target), drop = FALSE],
                   Sex = rep(gender, times = length(d[, 1])),
                   d[, (target + 1):length(d), drop = FALSE])
      }

      # change country codes
      d[, 1] <- toolCountry2isocode(d[, 1], ignoreCountries = c("Channel Islands", "World"))

      # remove NAs
      d <- d[-which(is.na(d[, 1])), ]

      # add "y"  in front of each the years
      d[, 2] <- paste(rep("y", times = length(d[, 2])), d[, 2], sep = "", collapse = NULL)

      # transform into magpie object
      out <- acast(d, Area ~ Year ~ Sex ~ Age ~ Education, value.var = names(d)[6])
      out <- as.magpie(out)
      out <- add_dimension(out, dim = 3.1, add = "Scenario", nm = scenario)
      merge[length(merge) + 1] <- out
    }
  }
  merge <- do.call(mbind, merge)
  getSets(merge) <- c("country", "year", "scenario", "sex", "age", "education")
  merge <- merge / 1000
  return(merge)
}
