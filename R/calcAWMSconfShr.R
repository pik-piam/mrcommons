#' @title calcAWMSconfShr
#' @description calculates the share of manure managed in different animal waste management systems in confinements.
#' Starting with IPCC 2005 values, turning into scenarios for the future.
#' @param rev revision number of madrat run
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky. Modifications by Edna J. Molina Bacca. Re-written by Michael Crawford.
#' @seealso
#' [calcAWMSconfShrPast()]
#' @examples
#' \dontrun{
#' calcOutput("AWMSconfShr")
#' }
#' @importFrom magpiesets findset

calcAWMSconfShr <- function(rev = 0.1) {

  # Define historical AWMS confinement shares
  past <- findset("past")
  out <- calcOutput("AWMSconfShrPast", aggregate = FALSE)
  out <- toolHoldConstantBeyondEnd(out)
  out <- add_dimension(out, dim = 3.1, nm = "constant")

  # This calculation will be weighted by the Mt Nr manure excreted
  manureExcreted <- collapseNames(calcOutput("Excretion", aggregate = FALSE)[, past, "confinement"][, , "nr"])
  manureExcreted <- toolHoldConstantBeyondEnd(manureExcreted)

  out <- toolAWMSScenarioCreation(name        = "transformation",
                                  startYear   = "y2010",
                                  targetYears = c("y2030", "y2050", "y2100"),
                                  targetAWMS  = c("digester", "lagoon"),
                                  valuesAWMS  = list(c(0.10, 0.90),
                                                     c(0.40, 0.50),
                                                     c(0.80, 0.10)),
                                  typeAWMS    = c("target", "relative_change"),
                                  out)

  out <- toolAWMSScenarioCreation(name        = "ssp1",
                                  startYear   = "y2010",
                                  targetYears = c("y2030", "y2050", "y2100"),
                                  targetAWMS  = c("digester", "lagoon"),
                                  valuesAWMS  = list(c(0.10, 0.90),
                                                     c(0.30, 0.60),
                                                     c(0.60, 0.30)),
                                  typeAWMS    = c("target", "relative_change"),
                                  out)

  out <- toolAWMSScenarioCreation(name        = "ssp2",
                                  startYear   = "y2010",
                                  targetYears = c("y2030", "y2050", "y2100"),
                                  targetAWMS  = c("digester"),
                                  valuesAWMS  = list(c(0.10),
                                                     c(0.30),
                                                     c(0.60)),
                                  typeAWMS    = c("target"),
                                  out)

  out <- toolAWMSScenarioCreation(name        = "ssp3",
                                  startYear   = "y2010",
                                  targetYears = c("y2030", "y2050", "y2100"),
                                  targetAWMS  = c("lagoon"),
                                  valuesAWMS  = list(c(1.11),
                                                     c(1.66),
                                                     c(3.33)),
                                  typeAWMS    = c("relative_change"),
                                  out)

  out <- toolAWMSScenarioCreation(name        = "ssp4",
                                  startYear   = "y2010",
                                  targetYears = c("y2030", "y2050", "y2100"),
                                  targetAWMS  = c("digester"),
                                  valuesAWMS  = list(c(0.10),
                                                     c(0.30),
                                                     c(0.60)),
                                  typeAWMS    = c("target"),
                                  out)

  out <- toolAWMSScenarioCreation(name        = "ssp5",
                                  startYear   = "y2010",
                                  targetYears = c("y2030", "y2050", "y2100"),
                                  targetAWMS  = c("digester", "lagoon"),
                                  valuesAWMS  = list(c(0.10, 1.11),
                                                     c(0.40, 1.66),
                                                     c(0.80, 3.33)),
                                  typeAWMS    = c("target", "relative_change"),
                                  out)

  # nolint start
  return(list(x           = out,
              weight      = manureExcreted,
              unit        = "share",
              description = paste("Share of excreted nitrogen within stables excreted in different animal waste management systems"),
              min         = 0,
              max         = 1))
  # nolint end
}
