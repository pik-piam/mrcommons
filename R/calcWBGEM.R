#' @title calcWBGEM
#' @description  recover world price of commodity in terms of real 2005 USD per metric ton
#' @return  magpie object of time series of world price of commodities
#' @author Xiaoxi Wang
#' @seealso
#' [madrat::readSource()],
#' [readWBGEM()]
#'
#' @examples
#' \dontrun{
#' calcOutput("WBGEM")
#' }
#'
#' @importFrom magclass mbind getNames setNames setYears

calcWBGEM <- function() {

  x     <- readSource("WBGEM")
  sugar <- grep("world", grep("Sugar", getNames(x), value = TRUE), value = TRUE)
  adjustFactor2010 <- setNames(x[, , grep("real", sugar, value = TRUE)] /
                                 setNames(x[, , grep("nominal", sugar, value = TRUE)], NULL), NULL)
  adjustFactor2005 <- 1 / setYears(x[, 2005, grep("real", sugar, value = TRUE)] /
                                     setNames(x[, 2005, grep("nominal", sugar, value = TRUE)], NULL), NULL) *
    adjustFactor2010
  adjustFactor2005 <- setNames(adjustFactor2005, NULL)

  vars <- grep("nominal", getNames(x), value = TRUE)
  tmp1 <- x[, , grep("[/]kg", vars, value = TRUE)] * 1000
  tmp2 <- x[, , grep("[/]mt", vars, value = TRUE)]
  tmp3 <- x[, , grep("Agr", vars, value = TRUE)]

  .rename <- function(x) {
    varName <- getNames(x)
    varName <- gsub("_nominal", "",
                    gsub("\\$", "",
                         gsub("[{^\\#&~_/<>'!,:.;`\"}@-]", "_",
                              gsub("[ /\t\n\r\f\v]", "", varName))))
    varName <- gsub("_kg", "", varName)
    varName <- gsub("_mt", "", varName)
    getNames(x) <- varName
    return(x)
  }

  out <- mbind(.rename(tmp1), .rename(tmp2), .rename(tmp3))
  out <- out * adjustFactor2005
  out[is.na(out)] <- 0

  # inflate to 2017 using US inflation for global value
  getItems(out, dim = 1) <- "USA"
  out <- GDPuc::toolConvertGDP(out, unit_in = "constant 2005 US$MER",
                               unit_out = "constant 2017 US$MER",
                               replace_NAs = "no_conversion")
  getItems(out, dim = 1) <- "GLO"

  return(list(x = out,
              unit = "real2017 USD per ton",
              weight = NULL,
              description = "WBGEM global price of commodity"))
}
