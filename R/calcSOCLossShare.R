#' @title calcSOCLossShare
#' @description Calculates soil organic carbon loss share on cellular level
#'
#' @param subsystems if FALSE just generic values will be used per climate zone ,
#'                   if TRUE crop specific values will be reported,
#'                   if aggregated crop specific factors will be aggregated using crop area
#' @param rate if change, change rates will be reported; if loss, loss rates will be reported
#' @param ipcc switch for different ipcc versions
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#'
#' \dontrun{
#' calcOutput("SOCLossShare", aggregate=FALSE)
#' }

calcSOCLossShare <- function(subsystems=FALSE, rate="change", ipcc="guide2006"){

  years                  <- findset("past")
  KG_climate             <- readSource("Koeppen", subtype="cellular", convert="onlycorrect")[,years,]
  KG2IPCC                <- toolGetMapping("mapping_koeppen_ipcc.csv", type="sectoral")
  getNames(KG_climate)   <- tolower(getNames(KG_climate))
  KG2IPCC$koeppen_geiger <- tolower(KG2IPCC$koeppen_geiger)
  year2climateClasses    <- c(guide2006="ipcc_reduced", guide2019="ipcc_reduced2019")
  climateClasses         <- toolSubtypeSelect(ipcc,year2climateClasses)

  IPCC_climate           <- toolAggregate(KG_climate, rel=KG2IPCC, from = "koeppen_geiger", to = climateClasses, dim=3)

  year2SCF               <- c(guide2006="SCF_sub", guide2019="SCF_sub2019")
  SCF                    <- toolSubtypeSelect(ipcc, year2SCF)
  SCFsub2IPCCclimate     <- readSource("IPCC",    subtype=SCF , convert=FALSE)[,,getNames(IPCC_climate)]

  if(subsystems==FALSE){

    SOCLossShare           <- dimSums(IPCC_climate * SCFsub2IPCCclimate[,,"maiz"], dim=3.1)
    getNames(SOCLossShare) <- "cshare"

  } else if(subsystems%in%c(TRUE, "aggregated")){

    SOCLossShare           <- dimSums(IPCC_climate * SCFsub2IPCCclimate, dim=3.1)

    if(subsystems=="aggregated"){
      MAGCrop      <- calcOutput("Croparea", physical=TRUE, cellular=TRUE, irrigation=FALSE, aggregate=FALSE)
      kcr2all      <- data.frame(list(kcr=getNames(SOCLossShare), all=rep("all",19)))
      SOCLossShare <- toolAggregate(SOCLossShare, weight=MAGCrop, rel=kcr2all, from="kcr", to="all", dim=3)
      getNames(SOCLossShare) <- "cshare"
    }
  }

  if(rate=="loss"){
    SOCLossShare <- 1- SOCLossShare
  } else if(rate!="change"){
    stop(paste(rate,"is unknown as 'rate' parameter specification."))
  }

  weight <- dimSums(calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, input_magpie = FALSE, years="y1995", round=6), dim=3)

  return(list(
    x            = SOCLossShare ,
    weight       = weight,
    unit         = "tC/tC",
    description  = "Soil organic carbon loss share per crop type",
    isocountries = FALSE))
}
