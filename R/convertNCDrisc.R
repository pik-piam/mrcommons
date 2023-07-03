#' @title convertNCDrisc
#' @description Converts data from the NCD risc consortium
#' body height:
#' Collaboration (NCD-RisC), NCD Risk Factor. 2016. "A Century of Trends in Adult Human Height." ELife 5 (July):e13410.
#' https://doi.org/10.7554/eLife.13410.
#' @param x unconverted magpie object from read-script
#' @param subtype "height" for body height data. Missing data is replaced by non-population weighted global average
#'
#' @return magpie object with a completed dataset.
#'
#' @seealso
#' [convertNCDrisc()]
#'
#' @importFrom magpiesets findset


convertNCDrisc <- function(x, subtype) {
  if (subtype == "height") {
    meanheight <- dimSums(x, dim = 1) / length(magclass::getItems(x, dim = 1))
    x <- toolCountryFill(x, fill = NA)
    countries <- where(is.na(x))$true$regions
    x[countries, , ] <- meanheight
    vcat(1, "better replace using function")

    return(x)

  } else if (subtype == "BMI") {
    mapping <- toolGetMapping(type = "sectoral", name = "NCDrisc2Lutz.csv", where = "mappingfolder")
    BMI <- new.magpie( # nolint: object_name_linter.
      cells_and_regions = magclass::getItems(x, dim = 1),
      years = getYears(x),
      names = c(paste0(unique(mapping$lutz), ".M"), paste0(unique(mapping$lutz), ".F")))

    for (i in getNames(BMI, dim = 1)) {
      item <- mapping$NCDrisc[mapping$lutz == i]
      BMI[, , i] <- dimSums(x[, , item], dim = "age") / length(item) # nolint: object_name_linter.
    }

    meanBMI <- dimSums(BMI, dim = 1) / length(magclass::getItems(BMI, dim = 1))
    BMI <- toolCountryFill(BMI, fill = NA) # nolint: object_name_linter.
    countries <- where(is.na(BMI))$true$regions
    BMI[countries, , ] <- meanBMI # nolint: object_name_linter.
    vcat(1, "better replace using function")
    return(BMI)

  } else if (subtype == "BMI_shr") {
    stop("use calcBMI_shr instead")
  } else if (subtype == "BMI_shr_underaged") {
    stop("use calcBMI_shr instead")
  }
}
