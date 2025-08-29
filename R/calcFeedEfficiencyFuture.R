#' calcFeedEfficiencyFuture
#' @description
#' Calculates future central feed shares for all livestock categories
#' based on the results of a non-linear regression between historical
#' central feed shares and livestock productivity and using Koeppen-
#' Geiger climate zones
#'
#'
#' @return Central feed shares and weights as list of two MAgPIE-objects
#' @author Isabelle Weindl, Benjamin Bodirsky, Stephen Wirth
#'
#' @examples
#' \dontrun{
#' calcOutput("FeedEfficiencyFuture")
#' }
#' @importFrom magclass magpie_expand
calcFeedEfficiencyFuture <- function() {
  # 1. feed efficiency regression coefficients a,b
  # 2. livestock productivity x

  # calc livst productivity
  lvstProd <- calcOutput("LivestockProductivity", aggregate = FALSE)

  # read regression coefficients for central feed shares
  feedEffRegr <- readSource("FeedEfficiencyReg")


  func <- function(x, a, b, z) {
    # x : stock or producer yield
    # a: Feed efficiency regression coefficient
    # b: Feed efficiency regression coefficient
    out <- a * x^b
    return(out)
  }

  # calculate feed efficiencies for 5 livestock commodities
  outEff <- new.magpie(cells_and_regions = getItems(lvstProd, dim = 1.1), years = getYears(lvstProd),
                        names = getNames(lvstProd), fill = NA, sets = getSets(lvstProd))

  outEff[, , "sys_dairy"] <- func(lvstProd[, , "sys_dairy"],
                                   a = feedEffRegr[, , "sys_dairy.A"],
                                   b = feedEffRegr[, , "sys_dairy.B"])
  outEff[, , "sys_beef"] <- func(lvstProd[, , "sys_beef"],
                                  a = feedEffRegr[, , "sys_beef.A"],
                                  b = feedEffRegr[, , "sys_beef.B"])
  outEff[, , "sys_pig"] <- func(lvstProd[, , "sys_pig"],
                                 a = feedEffRegr[, , "sys_pig.A"],
                                 b = feedEffRegr[, , "sys_pig.B"])
  outEff[, , "sys_hen"] <- func(lvstProd[, , "sys_hen"],
                                 a = feedEffRegr[, , "sys_hen.A"],
                                 b = feedEffRegr[, , "sys_hen.B"])
  outEff[, , "sys_chicken"] <- func(lvstProd[, , "sys_chicken"],
                                     a = feedEffRegr[, , "sys_chicken.A"],
                                     b = feedEffRegr[, , "sys_chicken.B"])

  # use livestock production as weight
  kli <- findset("kli")
  past <- findset("past_til2020")
  massbalance <- calcOutput("FAOmassbalance_pre", aggregate = FALSE)[, past, ]
  weight <- collapseNames(massbalance[, , kli][, , "dm"][, , "production"])

  mapping <- data.frame(
    kli = c("livst_pig", "livst_rum", "livst_chick", "livst_egg", "livst_milk"),
    sys = c("sys_pig", "sys_beef", "sys_chicken", "sys_hen", "sys_dairy"),
    stringsAsFactors = FALSE)

  weight <- rename_dimnames(weight, dim = 3, query = mapping, from = "kli", to = "sys")
  weight <- toolHoldConstantBeyondEnd(weight)

  out <- toolNAreplace(outEff, weight, replaceby = 0, val.rm = 0)

  return(list(x = out$x,
              weight = out$weight,
              unit = "ton DM per ton DM",
              description = "Feed efficiency for dairy cattle, beef cattle, pigs, hens and broilers"))
}
