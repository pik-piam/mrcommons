#' calcCentralFeedshares
#' @description
#' Calculates future central feed shares for all livestock categories
#' based on the results of a non-linear regression between historical
#' central feed shares and livestock productivity and using Koeppen-
#' Geiger climate zones
#'
#'
#' @return Central feed shares and weights as list of two MAgPIE-objects
#' @author Isabelle Weindl, Benjamin Bodirsky, Stephen Wirth, Jan Philipp Dietrich
#'
#' @examples
#' \dontrun{
#' calcOutput("CentralFeedshares")
#' }
calcCentralFeedshares <- function() {
  # 1. Koeppen input berechnen z
  # 2. feed share regression coefficients a,b
  # 3. livestock productivity x
  koeppen <- readSource("Koeppen") # read Koeppen
  map <- toolGetMapping(type = "sectoral", name = "KoeppenFeedbasketsmapping.csv",
                        where = "mappingfolder") # read mapping to Climate regions
  map$Koeppen <- paste0("kg_p_", map$Koeppen)
  koeppen <- koeppen[, , map$Koeppen]
  koeppen[is.na(koeppen)] <- 0 # set NAs to 0

  climatezones <- toolAggregate(koeppen, map, from = 1, to = 2, dim = 3) # Aggregate to climate zones
  climkg13 <- dimSums(climatezones[, , c("temp", "trop")], dim = 3) # Sum Climatezones to groups temp#trop
  climkg4 <-  climatezones[, , "cold"] # only cold climate

  # calc livst productivity
  lvstProd <- calcOutput("LivestockProductivity", aggregate = FALSE)

  # read regression coefficients for central feed shares
  feedShrRegr <- readSource("FeedShareReg")

  # specify systems for which regressions exist (exluding sys_hen and sys_chicken)
  systems <- c("sys_beef", "sys_dairy", "sys_pig")
  lvstProd <- lvstProd[, , systems]
  feedShrRegr <- feedShrRegr[, , systems]

  # set climate-specific factor for different production systems
  climk <- climkg13[, , rep(1, 3)]
  getNames(climk) <- systems
  climk[, , "sys_pig"] <- climkg4

  func <- function(x, a, b, z) {
    # x : stock or producer yield
    # a: FeedShare regression coefficient
    # b: FeedShare regression coefficient
    # z: aggregated climate-specific factor for each Magpie region
    out <- z * (1 - ((x * a)^3 / (0.1 + (x * a)^3))) + (1 - z) * (1 - ((x * b)^3 / (0.1 + (x * b)^3)))
    return(collapseNames(out))
    # difference in region of 1e-15
  }

  # calculate feedshares for livestock commodities
  outShr <- func(lvstProd, feedShrRegr[, , "A"], feedShrRegr[, , "B"], climk)

  # use livestock production as weight
  kl <- c("livst_pig", "livst_rum", "livst_milk")
  past <- findset("past_til2020")
  massbalance <- calcOutput("FAOmassbalance_pre", aggregate = FALSE)[, past, ]
  weight <- collapseNames(massbalance[, , kl][, , "dm"][, , "production"])

  mapping <- data.frame(
    kl = c("livst_pig", "livst_rum", "livst_milk"),
    sys = c("sys_pig", "sys_beef", "sys_dairy"),
    stringsAsFactors = FALSE)

  weight <- rename_dimnames(weight, dim = 3, query = mapping, from = "kl", to = "sys")
  weight <- toolHoldConstantBeyondEnd(weight)

  return(list(x = outShr,
              weight = weight,
              unit = "-",
              description = "Central feed shares for dairy cattle, beef cattle and pigs",
              min = 0,
              max = 1
  ))

}
