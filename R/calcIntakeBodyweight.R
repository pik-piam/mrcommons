#' @title calcIntakeBodyweight
#' @description it computes the food intake pro capita through the bodyweight
#' and the activity level. First it computes the basal metabolic rate (bmr) through
#' the Schofield equation and then the estimated energy required (eer) depending on
#' the activitiy level by FAO/WHO/UNU tables (Human Energy Requirments, Rome 2004)
#' @param bodyweight bodyweight in kg per capita or "standardized" for assuming standard values
#' @param bodyheight for mehthod FAO_WHO_UNU1985
#' @param tmean mean annual temperature
#' @param method method for calculating intake: either FAO_WHO_UNU1985 for estimates based on
#' height and bodyweight, schofield for just bodyweight, or HHS_USDA for recommended values for US-americans
#' @param inactivity Share of population inactive, provided as magpie object with different age groups
#' @author Eleonora Martinelli

calcIntakeBodyweight <- function(bodyweight, bodyheight = NULL, inactivity, tmean = NULL, method = NULL) {
  schoolkids <- c("5--9", "10--14", "15--19")

  if (method == "FAO_WHO_UNU1985") {

    pal <- inactivity * 1.53 + (1 - inactivity) * 1.76 # 2 being between active and vigourous
    # kids have a different inactivity criteria, so we increased physical activity for active kids
    pal[, , schoolkids]  <- inactivity[, , schoolkids]  * 1.53 + (1 - inactivity[, , schoolkids]) * 1.76

    bmr <- pal * 0
    bmr <- dimOrder(bmr, perm = c(2, 3, 1))

    ### Schofield equation Basal Metabolic Rate: bmr = S(age,sex)*weight + C(age,sex) ###

    parameters <- calcOutput("RegressionParameters", aggregate = FALSE, regression = "FAO_WHO_UNU1985")

    bmr <- (bodyweight * parameters[, , "weight"]
            + bodyheight / 100 * parameters[, , "height"]
            + parameters[, , "intercept"])
    requirement <- collapseNames(bmr, collapsedim = c(3, 4, 5)) * pal

  } else if (method == "HHS_USDA") {

    requirement <- readSource("HHS_USDA", convert = FALSE)
    weight <- requirement * 0 + 1
    mapping <- toolGetMapping(type = "sectoral", name = "HHS_USDA2hic.csv", where = "mappingfolder")
    requirement <- speed_aggregate(x = requirement, rel = mapping, weight = weight,
                                   from = "HHS_USDA", to = "hic", dim = 3.2)

    standardizedRequirement <- (collapseNames(requirement[, , "Sedentary"])
                                * inactivity + collapseNames(requirement[, , "Active"]) * (1 - inactivity))
    ### for kids, inactivity is defined differently (more than 60 minutes of physical activity)
    standardizedRequirement[, , schoolkids] <- (collapseNames(requirement[, , "Moderately_active"][, , schoolkids])
                                                * inactivity[, , schoolkids]
                                                + collapseNames(requirement[, , "Active"][, , schoolkids])
                                                * (1 - inactivity[, , schoolkids]))

    requirement <- standardizedRequirement
  } else if (method == "schofield") {
    # FAO/WHO/UNU. 2004. Human Energy Requirements.
    # http://www.fao.org/tempref/docrep/fao/007/y5686e/y5686e00.pdf.
    # http://www.fao.org/docrep/007/y5686e/y5686e07.htm

    pal <- inactivity * 1.53 + (1 - inactivity) * 1.76 # 2 being between active and vigourous
    # kids have a different inactivity criteria, so we increased physical activity for active kids
    pal[, , schoolkids]  <- inactivity[, , schoolkids]  * 1.53 + (1 - inactivity[, , schoolkids]) * 1.76

    bmr <- pal * 0
    bmr <- dimOrder(bmr, perm = c(2, 3, 1))

    ### Schofield equation Basal Metabolic Rate: bmr = S(age,sex)*weight + C(age,sex) ###

    schofield <- calcOutput("RegressionParameters", aggregate = FALSE, regression = "Schofield")

    bmr <- collapseNames(bodyweight * schofield[, , "slope"] + schofield[, , "intercept"], collapsedim = c(3, 4))
    requirement <- bmr * pal

  } else if (method == "Froehle") {

    pal <- inactivity * 1.53 + (1 - inactivity) * 1.76 # 2 being between active and vigourous
    # kids have a different inactivity criteria, so we increased physical activity for active kids
    pal[, , schoolkids]  <- inactivity[, , schoolkids]  * 1.53 + (1 - inactivity[, , schoolkids]) * 1.76

    bmr <- pal * 0
    bmr <- dimOrder(bmr, perm = c(2, 3, 1))

    ### Schofield equation Basal Metabolic Rate: bmr = S(age,sex)*weight + C(age,sex) ###

    parameters <- calcOutput("RegressionParameters", aggregate = FALSE, regression = "Froehle")

    bmr <- collapseNames(bodyweight * parameters[, , "weight"]
                         + tmean * parameters[, , "tmean"]
                         + parameters[, , "intercept"], collapsedim = c(3, 4, 5))
    requirement <- bmr * pal
  } else {
    stop("unknown method")
  }

  return(list(x = requirement,
              weight = NULL,
              unit = "kcal per capita per day",
              description = "Intake estimate",
              min = 500,
              max = 5000,
              isocountries = FALSE))
}
