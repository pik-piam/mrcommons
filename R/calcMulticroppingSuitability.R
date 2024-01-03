#' @title calcMulticroppingSuitability
#'
#' @description Calculates which grid cells are potentially suitable for
#'              multiple cropping activities under rainfed and irrigated conditions.
#'              Calculation is based on the length of the growing period determined by
#'              monthly grassland gross primary production (GPP).
#'
#' @param selectyears    Years to be returned
#' @param lpjml          LPJmL version required for respective inputs: natveg or crop
#' @param climatetype    Switch between different climate scenarios or
#'                       historical baseline "GSWP3-W5E5:historical"
#' @param suitability    "endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                       "exogenous": suitability for multiple cropping given by
#'                                    GAEZ data set
#' @param sectoral       "kcr" MAgPIE crops, and "lpj" LPJmL crops
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("MulticroppingSuitability", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass setYears getSets mbind getItems new.magpie
#'

calcMulticroppingSuitability <- function(selectyears, lpjml, climatetype,
                                         suitability = "endogenous", sectoral = "kcr") {
  # mappings
  lpj2mag  <- toolGetMapping("MAgPIE_LPJmL.csv",
                             type = "sectoral",
                             where = "mappingfolder")
  mapCell  <- toolGetMappingCoord2Country()

  # crop selection
  if (sectoral == "kcr") {

    # MAgPIE crops selected
    croplist <- unique(lpj2mag$MAgPIE)
    # remove pasture from croplist
    croplist <- croplist[croplist != "pasture"]
    # Crops that are not suitable for multiple cropping (either because they are a perennial
    # crop that's grown over several years or because the growing period is too long to
    # allow for another season
    cropsNoMC <- c("sugr_cane", "oilpalm", "betr", "begr")

  } else if (sectoral == "lpj") {

    # LPJmL crops selected
    croplist <- unique(lpj2mag$LPJmL)
    # remove mgrass from croplist
    croplist <- croplist[croplist != "mgrass"]
    # Crops that are not suitable for multiple cropping (either because they are a perennial
    # crop that's grown over several years or because the growing period is too long to
    # allow for another season
    cropsNoMC <- c("sugarcane", "betr", "begr")

  }

  # Prepare data structure as crop-specific object
  # (While the chosen rule is not crop-specific,
  #  crops that are not multiple cropping suitable are marked as such.
  #  This is done further down in the code)
  suitMC   <- new.magpie(cells_and_regions = paste(mapCell$coords, mapCell$iso, sep = "."),
                         years = selectyears,
                         names = paste(croplist,
                                       c(rep("rainfed", length(croplist)),
                                         rep("irrigated", length(croplist))),
                                       sep = "."),
                         fill = NA,
                         sets = c("x", "y", "iso", "year", "crop", "irrigation"))

  # Choose how multiple cropping suitability is determined
  if (suitability == "endogenous") {

    # Read in growing period months
    grper <- calcOutput("GrowingPeriodMonths",
                        selectyears = selectyears,
                        lpjml = lpjml, climatetype = climatetype,
                        aggregate = FALSE)

    # Calculate length of growing period
    lgp <- dimSums(grper, dim = "month")

    ### Multicropping Mask  ###
    ## Rule: Minimum length of growing period of 9 months
    suitMC[, , "rainfed"]   <- (lgp >= 9)[, , "rainfed"]
    suitMC[, , "irrigated"] <- (lgp >= 9)[, , "irrigated"]

  } else if (suitability == "exogenous") {

    ####################
    ### Read in data ###
    ####################
    mcZones <- calcOutput("MultipleCroppingZones", layers = 2, aggregate = FALSE)
    suitMC[, , "rainfed"]   <- mcZones[, , "rainfed"]
    suitMC[, , "irrigated"] <- mcZones[, , "irrigated"]

  } else {
    stop("Please select whether endogenously calculated multiple cropping suitability
         mask (endogenous) should be selected or whether
         GAEZ Multiple Cropping Zones data set should be used (exogenous)")
  }

  if (any(suitMC[, , sample(croplist, 1)] != suitMC[, , sample(croplist, 1)])) {
    stop("Multiple cropping suitability is not defined as crop-specific
          and should be the same for every crop.
          This is not the case here.
          Please double-check in mrland:calcMulticroppingSuitability")
  }

  # Crops that cannot be multiple cropped have suitability of 0
  suitMC[, , cropsNoMC] <- 0

  # If multiple cropping is possible under rainfed conditions,
  # it is also possible under irrigated conditions
  rfMC <- collapseNames(suitMC[, , "rainfed"])
  suitMC[, , "irrigated"][rfMC == 1] <- 1

  ##############
  ### Checks ###
  ##############
  if (any(is.na(suitMC))) {
    stop("calcMulticroppingSuitability produced NA values")
  }
  if (any(suitMC < 0)) {
    stop("calcMulticroppingSuitability produced negative values")
  }
  if (any(suitMC != 1 && suitMC != 0)) {
    stop("Problem in calcMulticroppingSuitability: Value should be 0 or 1!")
  }

  ##############
  ### Return ###
  ##############
  out         <- suitMC
  unit        <- "boolean"
  description <- paste0("Suitability for multicropping ",
                        "under irrigated and rainfed conditions. ",
                        "1 = suitable, 0 = not suitable")

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
