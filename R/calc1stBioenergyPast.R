#' @title calc1stBioenergyPast
#' @description
#' Calculates first generation biofuels production, imports, exports for biogas, bioethanol and biodiesel
#' from IEA database. The unit is Petajoule.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Xiaoxi Wang, Isabelle Weindl
#' @examples
#' \dontrun{
#' calcOutput("1stBioenergyPast")
#' }
#' @export

calc1stBioenergyPast <- function() {
  df <- toolCalcIEAfromStructureMappingPEFE(
    readSource("IEA", subtype = "EnergyBalances"),
    toolGetMapping(type = "sectoral", name = "structuremappingPE.csv",
                   where = "mrcommons", returnPathOnly = TRUE),
    subtype = "magpie")

  # Unit conversion from ktoe to PJ
  df <- df[, , c("biogas", "ethanol", "oils", "woodfuel")] * 0.041868

  # not yet implemented, but could be used to derive biogas yields:
  # https://www.ieabioenergy.com/wp-content/uploads/2011/10/Update_Energy_crop_2011.pdf

  return(list(x = df,
              weight = NULL,
              unit = "PJ",
              description = "1st generation bionegy demand")
  )
}
