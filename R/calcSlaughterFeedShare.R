#' @importFrom magclass dimOrder
#' @importFrom magpiesets findset

calcSlaughterFeedShare <- function(balanceflow = TRUE) {

  fbask                  <- calcOutput("FeedBaskets", aggregate = FALSE)
  getNames(fbask, dim = 1) <- paste0("alias_", getNames(fbask, dim = 1))
  attributes             <- calcOutput("Attributes", aggregate = FALSE)
  fbask                  <- dimSums(fbask * attributes, dim = 3.2)
  getNames(fbask, dim = 1) <- substring(getNames(fbask, dim = 1), 7)
  kli                      <- findset("kli")


  if (balanceflow) {

    fbaskbalance <- calcOutput("FeedBalanceflow", aggregate = FALSE, per_livestock_unit = TRUE)
    fbaskbalance <- dimSums(fbaskbalance * attributes, dim = 3.2)

    fbask        <- fbask + fbaskbalance[, , kli]

    # set overcorrected negative values to zero
    fbask[which(fbask < 0)] <- 0
  }

  slaughterFactor          <- collapseNames(calcOutput("Attributes", subtype = "SlaughterFactor",
                                                       aggregate = FALSE))[, , kli]
  attributesLivingAnimals <- calcOutput("Attributes", subtype = "LivingAnimals", aggregate = FALSE)[, , kli]

  weight <- fbask
  slaughterFeedShare <- slaughterFactor * attributesLivingAnimals / weight
  # limit to max 0.85
  slaughterFeedShare[slaughterFeedShare > 0.85] <- 0.85

  out <- toolNAreplace(slaughterFeedShare, weight + 1e-8)


  return(list(x = out$x,
              weight = out$weight,
              unit = "Share of DM,Nr,P,K,WM or gross energy",
              description = "Share of feed intake that gets withdrawn by slaughtermass per product",
              min = 0,
              max = 0.85))
}
