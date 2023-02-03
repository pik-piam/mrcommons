#' @title calcLPJmlCarbon
#' @description Handle LPJmL data for carbon module
#'
#' @param landtype    Switch between different land use types ("lu_maize_*", "lu_grass", "nat_veg")
#' @param subtype     Switch between diffrent input ("soil_layer", "mrh_*", "lit*", "vegc")
#'                    and also processed data (soilc_0-30, rh_0-30)
#' @param climatetype Switch between different climate scenarios ("historical")
#' @param selectyears defaults to past
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' [readLPJmlCarbon()]
#' @examples
#' \dontrun{
#' calcOutput("LPJmlCarbon", landtype = "nat_veg", subtype = "soilc_layer", aggregate = FALSE)
#' }
#'
calcLPJmlCarbon <- function(climatetype = "historical", landtype = "nat_veg",
                            subtype = "soilc_layer", selectyears = "past") {

  years <- sort(findset(selectyears, noset = "original"))

  #################
  ### Processed ###
  #################

  if (subtype %in% c("soilc_0-30")) {

    inputLPJmL <- calcOutput("LPJmlCarbon", climatetype = climatetype,
                              landtype = landtype, subtype = "soilc_layer",
                              selectyears = selectyears, aggregate = FALSE)

    weightLayers <- as.magpie(c(layer1 = 1,
                                 layer2 = 1 / 3,
                                 layer3 = 0,
                                 layer4 = 0,
                                 layer5 = 0))

    inputLPJmL   <- dimSums(inputLPJmL * weightLayers, dim = "layer")
    getNames(inputLPJmL, dim = "data") <- "soilc_0-30"

  } else if (subtype %in% c("soilc")) {

    inputLPJmL <- calcOutput("LPJmlCarbon", climatetype = climatetype,
                             landtype = landtype, subtype = "soilc_layer",
                             selectyears = selectyears, aggregate = FALSE)
    inputLPJmL <- dimSums(inputLPJmL, dim = "layer")
    getNames(inputLPJmL, dim = "data") <- "soilc"

  } else if (subtype %in% c("rh_0-30")) {

    rhLayer1 <- calcOutput("LPJmlCarbon", climatetype = climatetype,
                           landtype = landtype, subtype = "mrh_layer0",
                           selectyears = selectyears, aggregate = FALSE)
    rhLayer2 <- calcOutput("LPJmlCarbon", climatetype = climatetype,
                           landtype = landtype, subtype = "mrh_layer1",
                           selectyears = selectyears, aggregate = FALSE)

    # weighting to get 0-30cm
    weightLayers <- as.magpie(c(layer0 = 1, layer1 = 1 / 3))
    inputLPJmL   <- mbind(rhLayer1, rhLayer2)
    inputLPJmL   <- dimSums(inputLPJmL * weightLayers, dim = "layer")
    getNames(inputLPJmL, dim = "data") <- "rh_0-30"

  } else if (subtype %in% c("rh")) {

    rhLayer1 <- calcOutput("LPJmlCarbon", climatetype = climatetype,
                           landtype = landtype, subtype = "mrh_layer0",
                           selectyears = selectyears, aggregate = FALSE)
    rhLayer2 <- calcOutput("LPJmlCarbon", climatetype = climatetype,
                           landtype = landtype, subtype = "mrh_layer1",
                           selectyears = selectyears, aggregate = FALSE)
    rhLayer3 <- calcOutput("LPJmlCarbon", climatetype = climatetype,
                           landtype = landtype, subtype = "mrh_layer2",
                           selectyears = selectyears, aggregate = FALSE)
    rhLayer4 <- calcOutput("LPJmlCarbon", climatetype = climatetype,
                           landtype = landtype, subtype = "mrh_layer3",
                           selectyears = selectyears, aggregate = FALSE)
    rhLayer5 <- calcOutput("LPJmlCarbon", climatetype = climatetype,
                           landtype = landtype, subtype = "mrh_layer4",
                           selectyears = selectyears, aggregate = FALSE)

    inputLPJmL <- mbind(rhLayer1, rhLayer2, rhLayer3, rhLayer4, rhLayer5)
    inputLPJmL <- dimSums(inputLPJmL, dim = "layer")
    getNames(inputLPJmL, dim = "data") <- "rh"

  } else {
    #################
    ### Raw LPJmL ###
    #################

    if ("y2010" %in% years) {
      saveyears <- years
      if ("y2009" %in% years) {
        years <- years[1:(length(years) - 1)]
      } else              {
        years[length(years)] <- "y2009"
      }
      expandyears <- TRUE
    } else {
      expandyears <- FALSE
    }

    readinName <- paste0(climatetype, ".", landtype, ".", subtype)
    inputLPJmL <- readSource("LPJmlCarbon", subtype = readinName, convert = "onlycorrect")[, years, ]

    if (expandyears) {
      years      <- saveyears
      timeExpand <- setYears(inputLPJmL[, "y2009", ], "y2010")
      inputLPJmL <- mbind(inputLPJmL, timeExpand)[, years, ]
    }

    if (grepl("^m", subtype)) {
      # conversion to annual values for extensive quantities
      inputLPJmL   <- dimSums(inputLPJmL, dim = "month")
      getNames(inputLPJmL, dim = 3)  <- substring(getNames(inputLPJmL, dim = 3), 2)
      # Note: annual values as standard come without prefix
    }
  }

  ### weights as land surface area??? KRISTINE: Please clarify.
  # Landarea <- dimSums(calcOutput("LUH2v2", cellular=TRUE, selectyears=selectyears,aggregate= FALSE), dim=3) # nolint

  return(list(x            = inputLPJmL,
              weight       = NULL,
              unit         = "tC/ha",
              description  = paste0("Carbon output from LPJmL (",
                                    subtype, ") for ",
                                    climatetype, " climate and ",
                                    landtype, " landuse type."),
              isocountries = FALSE))

}
