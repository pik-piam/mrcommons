#' @title calcSOM
#' @description calculates Soil Organic Matter Pool, accounting for the management history.
#' We assume carbon Stocks from LPJml natural vegetation as a starting point.
#' Here we use the upper 30cm soil layer (0-20cm of + 1/3 of 30-50 cm).
#' We then correct carbon pools by lost c-share depending on the climate region, using
#' default factors of IPCC Guidelines 2006 table 5.5.
#' We assume that this IPCC-corrected value is the target long-term equilibrium value for the soil stocks.
#' Because soil decline and build-up slowly, we assume that in every year,
#' the carbon pools move 15% towards this new equilibrium.
#' This assumption is in line with IPCC saying that the process will take 20 years: with our assumption,
#' after 5 years 44% of the carbon pool is gone, after 10 years 80% and after 20 years 96%.
#' We determine a carbon stock for cropland soils and non-cropland soils in every cell.
#' If the cropland area expands, the carbon stock of noncropland is proportionally
#' assigned to the cropland pool and vice versa.
#' The outputs of the function are the soilc stocks for cropland and non-cropland.
#' Relevant for the release of N by SOM loss is also the change in carbon stocks per ha, as this relases or binds N.
#' This is done in delta cropland soilc.
#' @param climatetype Switch between different climate scenarios (default on "historical")
#' @param subtype "stock" (default) for absoulte values, "density" for per hectar values
#'
#' @return List of magpie object with results on country or cellular level,
#' weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @examples
#' \dontrun{
#' calcOutput("SOM")
#' }
#'
calcSOM <- function(climatetype = "historical", subtype = "stock") {

  startyear   <- 1951
  years       <- sort(findset("past_all"))
  spinup      <- paste0("y", (startyear:(as.integer(substring(years[1], 2)) - 1)))
  years       <- c(spinup, years)
  lengthyears <- length(years)

  soilc       <- calcOutput("LPJmlCarbon", climatetype = climatetype, landtype = "nat_veg",
                            subtype = "soilc_0-30", selectyears = "past_all", aggregate = FALSE)
  soilc       <- mbind(calcOutput("LPJmlCarbon", climatetype = climatetype, landtype = "nat_veg",
                                  subtype = "soilc_0-30", selectyears = spinup,     aggregate = FALSE),
                       soilc)

  states      <- toolCoord2Isocell(readSource("LUH2v2", subtype = "states", convert = "onlycorrect")[, years, ])

  cshare      <- toolCell2isoCell(readSource("LPJml_rev21", subtype = "cshare", convert = "onlycorrect"))
  attributes  <- new.magpie("GLO", NULL, c("c", "nr_available", "nr_loss"))
  attributes[, , ] <- c(1, 1 / 15, 0)

  # in principle possible to add begr/betr area based on LUH2v2
  # crpbf_c3per: C3 perennial crops grown as biofuels
  # crpbf_c4per: C4 perennial crops grown as biofuels

  crops  <- c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")
  cshare <- cshare * new.magpie("GLO", NULL, crops, fill = 1)

  # assumption that perrenial crops have constant c-content is dropped for now
  # until we classify the MAgPIE crops between annual and perrenials.
  # They cover anyway only a small area.
  # moreover, they are often characterized by low input (e.g. cotton), which leads to low-input c stock losses.

  cropArea    <- dimSums(states[, , crops], dim = 3)
  noncropArea <- dimSums(states, dim = 3) - cropArea

  targetCCrop    <- dimSums(soilc * (1 - cshare[, , crops]) * states[, , crops], dim = 3)
  targetCNoncrop <- dimSums(soilc * states[, , crops, invert = TRUE], dim = 3)

  transitions <- cropArea
  transitions[, years[2:length(years)], ] <- (cropArea[, years[2:length(years)], ]
                                              - setYears(cropArea[, years[seq_along(years) - 1], ],
                                                         years[2:length(years)]))

  abandonnedland <- newland <- transitions
  abandonnedland[abandonnedland > 0] <- 0
  abandonnedland <- abandonnedland * (-1)
  newland[newland < 0] <- 0

  cropC    <- cropCha    <- deltaCcrop    <- targetCCrop
  noncropC <- noncropCha <- deltaCnoncrop <- targetCNoncrop

  cropC[, 2:lengthyears, ]                <- NA
  noncropC[, 2:lengthyears, ]             <- NA

  cropCha[, , ]    <- deltaCcrop[, , ]    <- NA
  noncropCha[, , ] <- deltaCnoncrop[, , ] <- NA

  cropCha[, 1, ] <- cropC[, 1, ] / cropArea[, 1, ]
  cropCha[is.nan(cropCha)] <- 0

  noncropCha[, 1, ] <- noncropC[, 1, ] / noncropArea[, 1, ]
  noncropCha[is.nan(noncropCha)] <- 0

  for (yearX in (2:lengthyears)) {

    cropC[, yearX, ] <- (setYears(cropC[, yearX - 1, ], NULL)
                         + newland[, yearX, ] * setYears(noncropCha[, yearX - 1, ], NULL)
                         - abandonnedland[, yearX, ] * setYears(cropCha[, yearX - 1, ], NULL))

    noncropC[, yearX, ] <- (setYears(noncropC[, yearX - 1, ], NULL)
                            - newland[, yearX, ] * setYears(noncropCha[, yearX - 1, ], NULL)
                            + abandonnedland[, yearX, ] * setYears(cropCha[, yearX - 1, ], NULL))

    # assumption on transition: 15% of the soil difference per year. 44% after 5 years, 20% after 10 years,
    # 4% after 20 years

    deltaCcrop[, yearX, ]    <- (targetCCrop[, yearX, ] - cropC[, yearX, ]) * 0.15
    deltaCnoncrop[, yearX, ] <- (targetCNoncrop[, yearX, ] - noncropC[, yearX, ]) * 0.15

    # to avoid infs in division, a rounding is required

    cropC[, yearX, ]         <- round(cropC[, yearX, ] + deltaCcrop[, yearX, ], 10)
    noncropC[, yearX, ]      <- round(noncropC[, yearX, ] + deltaCnoncrop[, yearX, ], 10)

    cropCha[, yearX, ]       <- setYears(cropC[, yearX, ] / cropArea[, yearX, ], NULL)
    cropCha[is.nan(cropCha)] <- 0
    cropCha[abs(cropCha) == Inf] <- 0

    noncropCha[, yearX, ] <- setYears(noncropC[, yearX, ] / noncropArea[, yearX, ], NULL)
    noncropCha[is.nan(noncropCha)] <- 0
    noncropCha[abs(noncropCha) == Inf] <- 0
  }

  # delta_c is not equivalent to the difference in carbon_cropland_soils over time, as the area changes
  if (subtype == "stock") {
    out <- mbind(setNames(cropC, "cropland.soilc"),
                 setNames(noncropC, "noncropland.soilc"),
                 setNames(deltaCcrop, "cropland.delta_soilc"),
                 setNames(deltaCnoncrop, "noncropland.delta_soilc"),
                 setNames(targetCCrop, "cropland.target_soilc"),
                 setNames(targetCNoncrop, "noncropland.target_soilc"))

    unit    <- "Mt C"
    weight  <- NULL

  } else if (subtype == "density") {

    deltaCCropHa     <- toolNAreplace(deltaCcrop    / cropArea)$x
    deltaCNoncropHa  <- toolNAreplace(deltaCnoncrop / noncropArea)$x

    targetCCropHa    <- toolNAreplace(targetCCrop    / cropArea)$x
    targetCNoncropHa <- toolNAreplace(targetCNoncrop / noncropArea)$x


    out <- mbind(setNames(cropCha, "cropland.soilc"),
                 setNames(noncropCha, "noncropland.soilc"),
                 setNames(deltaCCropHa, "cropland.delta_soilc"),
                 setNames(deltaCNoncropHa, "noncropland.delta_soilc"),
                 setNames(targetCCropHa, "cropland.target_soilc"),
                 setNames(targetCNoncropHa, "noncropland.target_soilc"))

    unit    <- "t C per ha"
    weight  <- mbind(setNames(cropArea,    "cropland"),
                     setNames(noncropArea, "noncropland"))[, -c(1:10), ]

  } else {
    stop(paste("Subtype", subtype, "does not exist yet."))
  }

  # delete first 20 years of spin-up

  out <- out[, -c(1:10), ]

  return(list(
    x            = out,
    weight       = weight,
    unit         = unit,
    description  = paste("Carbon in cropland and non-cropland soils, as well as change",
                         "over time due to built-up or loss. Change is not equivalen to",
                         "the difference in carbon_cropland_soils over time, as the area changes."),
    isocountries = FALSE))
}
