#' @title calcTimberTradeBilateral
#' @description
#' A  very rough disaggregation of timber demand to bilateral trade flows
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @param products if "magpie" do UNIT (m3 --> MT) and name  conversion of the 2 magpie
#' wood products, else "FAO" gives original ones
#' @author David M Chen
#' @seealso
#' [mrfaocore::calcFAOmassbalance_pre()]
#' @examples
#' \dontrun{
#' calcOutput("TimberTradeBilateral")
#' }
#' @export

calcTimberTradeBilateral <- function(products = "magpie") {

  bilat <- readSource("FAOTradeMatrix", "import_qty_kforestry", convert = TRUE)
  # because the forestry matrix doesn't include many proeducts, including woodfuel completely,
  # we only take the sum of all products as general bilateral trade patterns to distribute the
  # unilateral trade across

  ## first conver the tonnes to m3 as in calcTimberDemand
  bilat[, , c("1860|Paper and paperboard, excluding newsprint",
              "1875|Wood pulp", "1671|Newsprint")] <-
    bilat[, , c("1860|Paper and paperboard, excluding newsprint",
                "1875|Wood pulp", "1671|Newsprint")] * 1000 / 450 ## 10^3 for t to kg. 450 for kg to m3
  getNames(bilat, dim = "ElementShort") <- gsub(pattern = "_m3",
                                                replacement = "",
                                                x = getNames(bilat, dim = "ElementShort"))
  bilat <- dimSums(bilat, dim = 3.1)
  bilatShr <- bilat / dimSums(bilat, dim  = 1.2)
  bilatShr[is.na(bilatShr)] <- 0
  timbDem <- calcOutput("TimberDemand", aggregate = FALSE)

  # hold future constant but remove the past
  yearsExt <- setdiff(getYears(timbDem),
                      getYears(bilatShr))[which(setdiff(getYears(timbDem, as.integer = TRUE),
                                                        getYears(bilatShr, as.integer = TRUE)) > 2015)]
  bilatShr <- toolHoldConstant(bilatShr, years = yearsExt)
  cyears <- intersect(getYears(bilatShr), getYears(timbDem))


  # rename exporters so that we distribute the  the importers across their own supply
  getItems(bilatShr, dim = 1.2) <- paste0(getItems(bilatShr, dim = 1.2), "2")

  # multiply
  out <- timbDem[, cyears, "import", drop = TRUE] * bilatShr
  getItems(out, dim = 1.2) <- gsub("[0-9]+", "", getItems(out, dim = 1.2))
  out <- collapseNames(out)

  getSets(out)[c(1, 2)] <- c("im", "ex")
  out[is.na(out)] <- 0
  out[is.infinite(out)] <- 0
  unit <- "mio m3"

  if (products == "magpie") {
    # convert to dry matter content, 0.6 and 0.3 respectively
    out[, , "Industrial roundwood"] <- out[, , "Industrial roundwood"] * 0.6
    out <- add_columns(out, dim = 3, addnm = "wood")
    out[, , "wood"] <- out[, , "Industrial roundwood"]

    out[, , "Wood fuel"] <- out[, , "Wood fuel"] * 0.3
    out <- add_columns(out, dim = 3, addnm = "woodfuel")
    out[, , "woodfuel"] <- out[, , "Wood fuel"]

    out <- out[, , c("wood", "woodfuel")]
    unit <- "mio T"
  }

  return(list(
    x = out,
    weight = NULL,
    min = 0,
    unit = unit,
    description = "Calculates bilateral timber trade based on historical FAO data"
  ))
}
